package ghcvm.runtime.types;

#include "Rts.h"

import java.util.ArrayList;
import static ghcvm.runtime.types.Task.InCall;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsMessages.*;
import static ghcvm.runtime.types.StgTSO.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.*;
import static ghcvm.runtime.types.StgTSO.ReturnCode.*;
import static ghcvm.runtime.RtsScheduler.RecentActivity.*;
import ghcvm.runtime.*;
import ghcvm.runtime.thread.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.exception.*;

public class Capability {
    public static int nCapabilities;
    public static int enabledCapabilities;
    public static Capability mainCapability;
    public static Capability lastFreeCapability;
    public static ArrayList<Capability> capabilities;
    public static int pendingSync;
    public static void init() {
#if defined(THREADED_RTS)
        nCapabilities = 0
        moreCapabilities(0, RtsFlags.ParFlags.nNodes);
        nCapabilities = RtsFlags.ParFlags.nNodes;
#else
        nCapabilities = 1;
        mainCapability = new Capability(0);
        capabilities.add(0, mainCapability);
#endif
        enabledCapabilities = nCapabilities;
        lastFreeCapability = capabilities.get(0);
    }

    public static void moreCapabilities (int from, int to) {
        ArrayList<Capability> oldCapabilities = capabilities;
        capabilities = new ArrayList<Capability>(to);
        if (to == 1) {
            mainCapability = new Capability(0);
            capabilities.add(0, mainCapability);
        } else {
            for (int i = 0; i < to; i++) {
                if (i < from) {
                    capabilities.add(i, oldCapabilities.get(i));
                } else {
                    capabilities.add(i, new Capability(i));
                }
            }
        }
    }

    public int no;
    public StgContext context = new StgContext();
    public Task runningTask;
    public boolean inHaskell = false;
    public int idle = 0;
    public boolean disabled = false;
    public StgTSO runQueueHead;
    public StgTSO runQueueTail;
    public InCall suspendedJavaCalls;
    public int contextSwitch;
    public int interrupt;
    public Task spareWorkers;
    public int nSpareWorkers;
    public Task returningTasksHead;
    public Task returningTasksTail;
    public Message inbox;
    public SparkPool sparks = new SparkPool();
    public SparkCounters sparkStats = new SparkCounters();
    public int ioManagerControlWrFd; // Not sure if this is necessary

    public Capability(int i) {
        this.no = i;
    }

    public void appendToRunQueue(StgTSO tso) {
        if (runQueueHead == null) {
            runQueueHead = tso;
            tso.blockInfo = null;
        } else {
            runQueueTail.link = tso;
            tso.link = runQueueTail;
        }
        runQueueTail = tso;
    }

    public Capability schedule(Task task) {
        Capability cap = this;
        while (true) {
            if (this.inHaskell) {
                errorBelch("schedule: re-entered unsafely.\n" +
                           "    Perhaps a 'foreign import unsafe' should be 'safe'?");
                System.exit(EXIT_FAILURE);
            }

            switch (schedulerState) {
                case SCHED_RUNNING:
                    break;
                case SCHED_INTERRUPTING:
                    // scheduleDoGC

                case SCHED_SHUTTING_DOWN:
                    if (!isBoundTask(task) && emptyRunQueue(cap)) {
                        return this;
                    }
                    break;
                default:
                    barf("sched state: " + schedulerState);
            }

            cap = cap.findWork();
#if defined(THREADED_RTS)
            cap.pushWork(task);
#endif
            cap = cap.detectDeadlock(task);
#if defined(THREADED_RTS)
            cap = cap.yield(task);
            if (emptyRunQueue(cap)) continue;
#endif
            StgTSO t = cap.popRunQueue();
#if defined(THREADED_RTS)
            InCall bound = t.bound;
            if (bound != null) {
                if (bound.task != task) {
                    cap.pushOnRunQueue(t);
                    continue;
                }
            } else {
                if (task.incall.tso) {
                    cap.pushOnRunQueue(t);
                    continue;
                }
            }
#endif
            if (schedulerState >= SCHED_INTERRUPTING &&
                !(t.whatNext == ThreadComplete || t.whatNext == ThreadKilled)) {
                cap.deleteThread(t);
            }
#if defined(THREADED_RTS)
               if (cap.disabled && t.bound != null) {
                   Capability destCap = capabilities[cap.no % enabledCapabilities];
                   cap.migrateThread(t, destCap);
                   continue;
               }
#endif
            if (RtsFlags.ConcFlags.ctxtSwitchTicks == 0
                && !emptyThreadQueues(cap)) {
                cap.contextSwitch = 1;
            }

            cap.context.currentTSO = t;
            cap.context.myCapability = cap;
            cap.interrupt = 0;
            cap.inHaskell = true;
            cap.idle = 0;

            WhatNext prevWhatNext = t.whatNext;
            int errno = t.savedErrno;

            switch (recentActivity) {
                case DoneGC:
                    // stuff here
                    break;
                case Inactive:
                    break;
                default:
                    recentActivity = Yes;
            }

            ReturnCode ret;
            switch (prevWhatNext) {
                case ThreadKilled:
                case ThreadComplete:
                    ret = ThreadFinished;
                    break;
                case ThreadRunGHC:
                    StgContext context = cap.context;
                    context.currentTSO = t;
                    context.myCapability = cap;
                    try {
                        t.stackBottom.enter(cap.context);
                    } catch (ThreadYieldException e) {

                    } finally {
                        ret = context.ret;
                    }
                    break;
                case ThreadInterpret:
                    // TODO: Implement ghci
                    break;
                default:
                    barf("schedule: invalid what_next field");
            }

            cap.inHaskell = false;
            t = context.currentTSO;
            // t = cap -> r.rCurrentTSO;
            // more stuff to implement
            return cap;
        }
    }

    public void migrateThread(StgTSO tso, Capability destCap) {}

    public void deleteThread(StgTSO tso) {
        if (tso.whyBlocked != BlockedOnCCall &&
            tso.whyBlocked != BlockedOnCCall_Interruptible) {
            //tso.cap.throwToSingleThreaded(tso, null);
        }
    }

    public void pushOnRunQueue(StgTSO tso) {
        tso.link = runQueueHead;
        tso.blockInfo = null;
        if (runQueueHead != null) {
            runQueueHead.blockInfo = tso;
        }
        runQueueHead = tso;
    }

    public StgTSO popRunQueue() {
        StgTSO t = runQueueHead;
        runQueueHead = t.link;
        if (t.link != null) {
            t.link.blockInfo = null;
        }
        t.link = null;
        if (runQueueHead == null) {
            runQueueTail = null;
        }
        return t;
    }

    public Capability yield(Task task) {return this;}

    public void pushWork(Task task) {}
    public Capability detectDeadlock(Task task) {return this;}

    public Capability findWork() {
        Capability cap = this;
        startSignalHandlers();
        cap = processInbox();
        checkBlockedThreads();

#if defined(THREADED_RTS)
        if (emptyRunQueue(this)) activateSpark();
#endif
        return cap;
    }

    public void activateSpark() {}

    public void startSignalHandlers() {
#if defined(RTS_USER_SIGNALS) && !defined(THREADED_RTS)
               if (RtsFlags.MiscFlags.installSignalHandlers /* && signals_pending() */) {
                   // start signal handlers
               }
#endif
    }

    public void checkBlockedThreads() {
#if !defined(THREADED_RTS)
                if (!emptyQueue(blockedQueueHead) || !emptyQueue(sleepingQueue)) {
                    //               awaitEvent (emptyRunQueue(this));
                    // TODO: Implement this with IO Manager
                }
#endif
    }

    public Capability processInbox() {return this;}

    public void startWorkerTask() {
        Task task = new Task(true);
        // TODO: Verify that this synchronization is doing as desired
        synchronized (task) {
            task.cap = this;
            this.runningTask = task;
            WorkerThread thread = new WorkerThread(task);
            thread.start();
            task.id = thread.getId();
        }
    }
}
