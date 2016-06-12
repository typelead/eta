package ghcvm.runtime.types;

#include "Rts.h"

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Deque;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import static ghcvm.runtime.types.Task.InCall;
import static ghcvm.runtime.RtsMessages.*;
import static ghcvm.runtime.RtsStartup.*;
import static ghcvm.runtime.RtsStartup.ExitCode.*;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsScheduler.SchedulerState.*;
import static ghcvm.runtime.types.StgTSO.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.*;
import static ghcvm.runtime.closure.StgContext.*;
import static ghcvm.runtime.closure.StgContext.ReturnCode.*;
import static ghcvm.runtime.RtsScheduler.RecentActivity.*;
import ghcvm.runtime.*;
import ghcvm.runtime.thread.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.exception.*;
import ghcvm.runtime.prim.*;
import ghcvm.runtime.message.*;
import ghcvm.runtime.stackframe.*;

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
    public Lock lock = new ReentrantLock();
    public StgContext context = new StgContext();
    public Task runningTask;
    public boolean inHaskell;
    public int idle;
    public boolean disabled;
    public Queue<StgTSO> runQueue = new ArrayDeque<StgTSO>();
    public InCall suspendedJavaCalls;
    public boolean contextSwitch;
    public boolean interrupt;
    public Deque<Task> spareWorkers = new ArrayDeque<Task>();
    public Deque<Task> returningTasks = new ArrayDeque<Task>();
    public Deque<Message> inbox = new ArrayDeque<Message>();
    public SparkPool sparks = new SparkPool();
    public SparkCounters sparkStats = new SparkCounters();
    public List<StgWeak> weakPtrList = new ArrayList<StgWeak>();
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
                stgExit(EXIT_FAILURE);
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

            if (schedulerState.compare(SCHED_INTERRUPTING) >= 0 &&
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
                cap.contextSwitch = true;
            }

            cap.context.currentTSO = t;
            cap.context.myCapability = cap;
            cap.interrupt = false;
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

                    } catch (StgReturnException e) {
                        // Ensure that the StgContext objects match.
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

    public void migrateThread(StgTSO tso, Capability to) {
        tso.whyBlocked = ThreadMigrating;
        tso.cap = to;
        tryWakeupThread(tso);
    }

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
    public void threadPaused(StgTSO tso) {
        maybePerformBlockedException(tso);
        if (tso.whatNext == ThreadKilled) return;
        // TODO: Implement rest
    }

    public boolean maybePerformBlockedException(StgTSO tso) {
        if (tso.whatNext == ThreadComplete /* || tso.whatNext == ThreadFinished */) {
            if (tso.blockedExceptions.isEmpty()) {
                return false;
            } else {
                awakenBlockedExceptionQueue(tso);
                return true;
            }
        }

        if (!tso.blockedExceptions.isEmpty() && (tso.flags & TSO_BLOCKEX) != 0) {
            // debugTraceCap(DEBUG_SCHED, this, "throwTo: thread %d has blocked exceptions but is inside block", tso.id);
        }

        if (!tso.blockedExceptions.isEmpty() && ((tso.flags & TSO_BLOCKEX) == 0 || ((tso.flags & TSO_INTERRUPTIBLE) != 0 && tso.interruptible()))) {
            while (true) {
                MessageThrowTo msg = tso.blockedExceptions.pollFirst();
                if (msg == null) return false;
                //locking and unlocking closures here
                throwToSingleThreaded(msg.target, msg.exception);
                StgTSO source = msg.source;
                msg.done();
                tryWakeupThread(source);
                return true;
            }
        }
        return false;
    }

    public void awakenBlockedExceptionQueue(StgTSO tso) {

    }
    public void tryWakeupThread(StgTSO tso) {
        if (RtsFlags.ModeFlags.threaded) {
            if (tso.cap != this) {
                MessageWakeup msg = new MessageWakeup(tso);
                sendMessage(tso.cap, msg);
                // debugTraceCap
                return;
            }
        }

        switch (tso.whyBlocked) {
            case BlockedOnMVar:
            case BlockedOnMVarRead:
                if (!tso.inMVarOperation) {
                    tso.blockInfo = null;
                    break;
                } else {
                    return;
                }
            case BlockedOnMsgThrowTo:
                // Do some locking and unlocking
                // The top of the stack should be blockThrowTo
                tso.stack.pop();
                break;
            case BlockedOnBlackHole:
            case BlockedOnSTM:
            case ThreadMigrating:
                break;
            default:
                return;
        }
        tso.whyBlocked = NotBlocked;
        appendToRunQueue(tso);
        // can context switch now
    }

    public void sendMessage(Capability target, Message msg) {
        // acquire target lock;
        Lock lock = target.lock;
        lock.lock();
        try {
            target.inbox.offerFirst(msg);
            if (target.runningTask == null) {
                target.runningTask = Task.myTask();
                target.release_(false);
            } else {
                target.interrupt();
            }
        } finally {
            lock.unlock();
        }
    }

    public void release_(boolean alwaysWakeup) {
        Task task = runningTask;
        runningTask = null;
        Task workerTask = returningTasks.pollFirst();
        if (workerTask != null) {
            giveCapabilityToTask(workerTask);
            return;
        }
        /* TODO: Evaluate if this is required
        if (pendingSync != 0 && pendingSync != SYNC_GC_PAR) {
            lastFreeCapability = this;
            //debugTrace
            return;
            }*/
        StgTSO nextTSO = runQueue.peek();
        if (nextTSO.bound != null) {
            task = nextTSO.bound.task;
            giveCapabilityToTask(task);
            return;
        }

        if (spareWorkers.isEmpty()) {
            if (schedulerState.compare(SCHED_SHUTTING_DOWN) < 0 || !emptyRunQueue()) {
                //debugTrace
                startWorkerTask();
                return;
            }
        }

        if (alwaysWakeup || !emptyRunQueue() || !emptyInbox()
            || (!disabled && !emptySparkPool()) || globalWorkToDo()) {
            task = spareWorkers.pollFirst();
            if (task != null) {
                giveCapabilityToTask(task);
                return;
            }
        }

        lastFreeCapability = this;
        //debugTrace;
    }

    public void release() {
        lock.lock();
        try {
            release_(false);
        } finally {
            lock.unlock();
        }
    }

    public void releaseAndWakeup() {
        lock.lock();
        try {
            release_(true);
        } finally {
            lock.unlock();
        }
    }

    public void giveCapabilityToTask(Task task) {
        // debugTrace
        Lock lock = task.lock;
        lock.lock();
        try {
            if (!task.wakeup) {
                task.wakeup = true;
                //signalCondition
            }
        } finally {
            lock.unlock();
        }
    }

    public boolean emptyInbox() {
        return inbox.isEmpty();
    }

    public boolean emptySparkPool() {
        return sparks.isEmpty();
    }

    public boolean globalWorkToDo() {
        return (schedulerState.compare(SCHED_INTERRUPTING) >= 0)
            || recentActivity == Inactive;
    }

    public void interrupt() {
        stop();
        interrupt = false;
    }

    public void contextSwitch() {
        stop();
        contextSwitch = true;
    }

    public void stop() {
        // Find a method to force the stopping of a thread;
    }

    public void throwToSingleThreaded(StgTSO tso, StgClosure exception) {
        throwToSingleThreaded__(tso, exception, false, null);
    }

    public void throwToSingleThreaded_(StgTSO tso, StgClosure exception,
                                       boolean stopAtAtomically) {
        throwToSingleThreaded__(tso, exception, stopAtAtomically, null);
    }

    public void throwToSingleThreaded__(StgTSO tso, StgClosure exception,
                                        boolean stopAtAtomically,
                                        StgUpdateFrame stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }

        removeFromQueues(tso);
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public void suspendComputation(StgTSO tso, StgUpdateFrame stopHere) {
        throwToSingleThreaded__(tso, null, false, stopHere);
    }

    public void removeFromQueues(StgTSO tso) {
        switch (tso.whyBlocked) {
            case NotBlocked:
            case ThreadMigrating:
                return;
            case BlockedOnSTM:
                break;
            case BlockedOnMVar:
            case BlockedOnMVarRead:
                tso.removeFromMVarBlockedQueue();
                break;
            case BlockedOnBlackHole:
                break;
            case BlockedOnMsgThrowTo:
                MessageThrowTo m = (MessageThrowTo) tso.blockInfo;
                m.done();
                break;
            case BlockedOnRead:
            case BlockedOnWrite:
                // TODO: Remove the following check if this state never occurs
                // if the threaded rts
                if (RtsFlags.ModeFlags.threaded) {
                    blockedQueue.remove(tso);
                }
                break;
            case BlockedOnDelay:
                sleepingQueue.remove(tso);
                break;
            default:
                barf("removeFromQueues: %d", tso.whyBlocked);
        }
        tso.whyBlocked = NotBlocked;
        appendToRunQueue(tso);
    }

    public void raiseAsync(StgTSO tso, StgClosure exception,
                           boolean stopAtAtomically, StgUpdateFrame stopHere) {
        //debugTrace
        // TODO: Implement later
    }

    public boolean messageBlackHole(MessageBlackHole msg) {
        // TODO: Implement
        return false;
    }

    public void checkBlockingQueues(StgTSO tso) {

    }

    public void updateThunk(StgTSO tso, StgInd thunk, StgClosure val) {
        thunk.updateWithIndirection(val);
        if (thunk.isBlackHole()) {
            thunk.indirectee.thunkUpdate(this, tso);
        }
    }

    public void wakeBlockingQueue(StgBlockingQueue blockingQueue) {
        Iterator<MessageBlackHole> it = blockingQueue.iterator();
        while (it.hasNext()) {
            MessageBlackHole msg = it.next();
            if (msg.isValid()) tryWakeupThread(msg.tso);
        }
        // do cleanup here to destroy the BQ;
    }
}
