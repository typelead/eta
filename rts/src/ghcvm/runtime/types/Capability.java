package ghcvm.runtime.types;

#include "Rts.h"

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Queue;
import java.util.ArrayDeque;
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
    public static List<Capability> capabilities;
    public static int pendingSync;
    public static void init() {
        if (RtsFlags.ModeFlags.threaded) {
            nCapabilities = 0;
            moreCapabilities(0, RtsFlags.ParFlags.nNodes);
            nCapabilities = RtsFlags.ParFlags.nNodes;
        } else {
            nCapabilities = 1;
            mainCapability = new Capability(0);
            capabilities.add(0, mainCapability);
        }
        enabledCapabilities = nCapabilities;
        lastFreeCapability = capabilities.get(0);
    }

    public static void moreCapabilities (int from, int to) {
        ArrayList<Capability> oldCapabilities = (ArrayList<Capability>) capabilities;
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
    public boolean inHaskell;
    public int idle;
    public boolean disabled;
    public Queue<StgTSO> runQueue = new ArrayDeque<StgTSO>();
    public InCall suspendedJavaCalls;
    public int contextSwitch;
    public int interrupt;
    public Task spareWorkers;
    public int nSpareWorkers;
    public List<Task> returningTasks = new ArrayList<Task>();
    public Message inbox;
    public SparkPool sparks = new SparkPool();
    public SparkCounters sparkStats = new SparkCounters();
    public int ioManagerControlWrFd; // Not sure if this is necessary

    public Capability(int i) {
        this.no = i;
    }

    public void appendToRunQueue(StgTSO tso) {
        runQueue.add(tso);
        /* TODO: Is this line necessary?
        if (runQueue.isEmpty()) tso.blockInfo = null;
        */
    }

    public Capability schedule(Task task) {
        Capability cap = this;
        boolean threaded = RtsFlags.ModeFlags.threaded;
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
                    if (!task.isBound() && cap.emptyRunQueue()) {
                        return this;
                    }
                    break;
                default:
                    barf("sched state: " + schedulerState);
            }

            cap = cap.findWork();
            if (threaded) {
                cap.pushWork(task);
            }
            cap = cap.detectDeadlock(task);
            if (threaded) {
                cap = cap.yield(task);
                if (cap.emptyRunQueue()) continue;
            }

            StgTSO t = cap.popRunQueue();
            if (threaded) {
                InCall bound = t.bound;
                if (bound != null) {
                    if (bound.task != task) {
                        cap.pushOnRunQueue(t);
                        continue;
                    }
                } else {
                    if (task.incall.tso != null) {
                        cap.pushOnRunQueue(t);
                        continue;
                    }
                }
            }

            if (schedulerState >= SCHED_INTERRUPTING &&
                !(t.whatNext == ThreadComplete || t.whatNext == ThreadKilled)) {
                cap.deleteThread(t);
            }

            if (threaded) {
                if (cap.disabled && t.bound != null) {
                    Capability destCap = capabilities.get(cap.no % enabledCapabilities);
                    cap.migrateThread(t, destCap);
                    continue;
                }
            }

            if (RtsFlags.ConcFlags.ctxtSwitchTicks == 0
                && !cap.emptyThreadQueues()) {
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
                    Iterator<StackFrame> it = t.stack.descendingIterator();
                    context.it = it;
                    try {
                        it.next().enter(context);
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
        /* TODO: Take care of blockInfo later */
        runQueue.add(tso);
    }

    public StgTSO popRunQueue() {
        return runQueue.remove();
        /* runQueue.size() > 1 -> blockInfo = null of the removed element */
    }

    public Capability yield(Task task) {return this;}

    public void pushWork(Task task) {}
    public Capability detectDeadlock(Task task) {return this;}

    public Capability findWork() {
        Capability cap = this;
        startSignalHandlers();
        cap = processInbox();
        checkBlockedThreads();

        if (RtsFlags.ModeFlags.threaded) {
            if (emptyRunQueue()) activateSpark();
        }
        return cap;
    }

    public void activateSpark() {}

    public void startSignalHandlers() {
        if (!RtsFlags.ModeFlags.threaded && RtsFlags.ModeFlags.userSignals) {

            if (RtsFlags.MiscFlags.installSignalHandlers /* && signals_pending() */) {
                // start signal handlers
            }
        }
    }

    public void checkBlockedThreads() {
        if (!RtsFlags.ModeFlags.threaded) {
            if (!emptyBlockedQueue() || !emptySleepingQueue()) {
                //               awaitEvent (emptyRunQueue(this));
                // TODO: Implement this with IO Manager
            }
        }
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

    public boolean emptyRunQueue() {
        return runQueue.isEmpty();
    }

    public boolean emptyThreadQueues() {
        // TODO: More optimal way of representing this?
        if (RtsFlags.ModeFlags.threaded) {
            return emptyRunQueue();
        } else {
            return emptyRunQueue() && emptyBlockedQueue() && emptySleepingQueue();
        }
    }
}
