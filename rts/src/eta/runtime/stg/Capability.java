package eta.runtime.stg;

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Deque;
import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.atomic.AtomicReference;
import eta.runtime.stg.Task.InCall;
import static eta.runtime.Rts.*;
import static eta.runtime.RtsMessages.*;
import static eta.runtime.Rts.ExitCode.*;
import static eta.runtime.RtsScheduler.*;
import static eta.runtime.RtsScheduler.SchedulerState.*;
import static eta.runtime.RtsScheduler.SchedulerStatus.*;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.stg.StgContext.*;
import static eta.runtime.stg.StgContext.ReturnCode.*;
import static eta.runtime.RtsScheduler.RecentActivity.*;
import static eta.runtime.interpreter.Interpreter.*;
import eta.runtime.*;
import eta.runtime.thunk.*;
import eta.runtime.thread.*;
import eta.runtime.stg.*;
import eta.runtime.exception.*;
import eta.runtime.stm.*;
import eta.runtime.message.*;
import eta.runtime.concurrent.*;
import eta.runtime.parallel.*;
import eta.runtime.apply.*;
import static eta.runtime.stg.StackFrame.MarkFrameResult;
import static eta.runtime.stg.StackFrame.MarkFrameResult.*;
import static eta.runtime.stm.StgTRecChunk.TREC_CHUNK_NUM_ENTRIES;
import static eta.runtime.stm.STM.EntrySearchResult;
import static eta.runtime.stm.TRecState.*;

public final class Capability {
    public static final int MAX_SPARE_WORKERS = 6;
    public static int nCapabilities;
    public static int enabledCapabilities;
    public static Capability mainCapability;
    public static Capability lastFreeCapability;
    public static List<Capability> capabilities = new ArrayList<Capability>(1);
    public static SyncType pendingSync = SyncType.SYNC_NONE;

    public enum SyncType {
        SYNC_NONE(0),
        SYNC_GC_SEQ(1),
        SYNC_GC_PAR(2),
        SYNC_OTHER(3);

        private int type;
        SyncType(int type) {
            this.type = type;
        }
    }

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
    public volatile boolean inStg;
    public int idle;
    public volatile boolean disabled;
    public Deque<TSO> runQueue = new ConcurrentLinkedDeque<TSO>();
    public Deque<InCall> suspendedJavaCalls = new ArrayDeque<InCall>();
    public volatile boolean contextSwitch;
    public volatile boolean interrupt;
    public Queue<Task> spareWorkers = new ArrayBlockingQueue<Task>(MAX_SPARE_WORKERS);
    public Deque<Task> returningTasks = new ArrayDeque<Task>();
    public Deque<Message> inbox = new ArrayDeque<Message>();
    public Deqeue<Closure> sparks = new ConcurrentLinkedDeque<Closure>();

    public SparkCounters sparkStats = new SparkCounters();
    public List<Weak> weakPtrList = new ArrayList<Weak>();

    /* STM-related data structures */
    public Stack<StgTRecChunk> freeTRecChunks = new Stack<StgTRecChunk>();
    public Queue<InvariantCheck> freeInvariantChecksQueue = new ArrayDeque<InvariantCheck>();
    public Queue<TransactionRecord> freeTRecHeaders = new ArrayDeque<TransactionRecord>();
    public long transactionTokens;

    public int ioManagerControlWrFd; /* TODO: Finish implementation of IO manager */

    public Capability(int i) {
        this.no = i;
    }

    public final Capability schedule(Task task) {
        Capability cap = this;
        Closure result;
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("Capability[%d]: schedule()", cap.no);
        }
        while (true) {
            if (cap.inStg) {
                errorBelch("{Scheduler} Re-entered unsafely.\n" +
                           "    Perhaps a 'foreign import unsafe' should be 'safe'?");
                stgExit(EXIT_FAILURE);
            }

            switch (schedulerState) {
                case SCHED_RUNNING:
                    break;
                case SCHED_INTERRUPTING:
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} Interrupting!");
                    }
                case SCHED_SHUTTING_DOWN:
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} Shutting Down!");
                    }
                    if (!task.isBound() && cap.emptyRunQueue()) {
                        return cap;
                    }
                    break;
                default:
                    barf("FATAL ERROR: Invalid scheduler state: " + schedulerState);
            }
            cap.findWork();
            /* TODO: The following still need to be implemented:
                     - Work stealing algorithm for TSOs - can be implemented as part of findWork().
                     - Deadlock detection. Be able to detect <<loop>>.
                     - Killing TSO on shutdown.
                     - Migrate threads when disabled?
            */

            // todo: yielding is causing unnecessary work for all the capabilities.
            //       disabling until we find a need for it.
            // cap = cap.scheduleyield(task);
            if (cap.emptyRunQueue()) continue;

            TSO t = cap.popRunQueue();
            Task.InCall bound = t.bound;
            if (bound != null) {
                Task boundTask = bound.task();
                if (boundTask != task) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} TSO[%d] is bound to Task[%d], but current Task[%d].", t.id, boundTask.id, task.id);
                    }
                    cap.pushOnRunQueue(t);
                    continue;
                }
            } else {
                if (task.incall.tso != null) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} Current Task[%d] cannot run TSO[%d].", task.id, t.id);
                    }
                    cap.pushOnRunQueue(t);
                    continue;
                }
            }

            StgContext context = cap.context;
            context.reset(cap, t);
            cap.inStg = true;

            WhatNext prevWhatNext = t.whatNext;
            switch (prevWhatNext) {
                case ThreadKilled:
                case ThreadComplete:
                    ret = ThreadFinished;
                    break;
                case ThreadRun:
                    try {
                        result = tso.closure.enter(context);
                    } catch (Exception e) {
                        // TODO: Catch exceptions here?
                        throw e;
                    } finally {
                        ret = context.ret;
                        cap = context.myCapability;
                    }
                    break;
                case ThreadInterpret:
                    cap = interpretBCO(cap);
                    ret = cap.context.ret;
                    break;
                default:
                    barf("{Scheduler} Invalid whatNext field for TSO[%d].", t.id);
            }

            cap.inStg = false;
            cap.context.currentTSO = null;

            /* TODO: Replace this check elsewhere. It's to detect loops in STM. */
            if (t.trec != null && t.whyBlocked == NotBlocked) {
                if (!stmValidateNestOfTransactions(t.trec)) {
                    throwToSingleThreaded_(t, null, true);
                }
            }

            awakenBlockedExceptionQueue(t);

            if (t.bound != null) {
                if (t.bound != task.incall) {
                    barf("{Scheduler} Finished running a Bound TSO[%d] not bound to me.", t.id);
                }
                assert task.incall.tso == t;

                if (t.whatNext == ThreadComplete) {
                    task.incall.ret = result;
                    task.incall.returnStatus = Success;
                } else {
                    if (task.incall.ret != null) {
                        task.incall.ret = null;
                    }
                    if (schedulerState.compare(SCHED_INTERRUPTING) >= 0) {
                        task.incall.returnStatus = Interrupted;
                    } else {
                        task.incall.returnStatus = Killed;
                    }
                }
                t.bound = null;
                task.incall.tso = null;
            }

            return cap;
        }
    }

    public final void migrateThread(TSO tso, Capability to) {
        tso.whyBlocked = ThreadMigrating;
        tso.cap = to;
        tryWakeupThread(tso);
    }

    public final void appendToRunQueue(TSO tso) {
        runQueue.offer(tso);
    }

    public final void pushOnRunQueue(TSO tso) {
        runQueue.offerFirst(tso);
    }

    public final TSO popRunQueue() {
        return runQueue.poll();
    }

    public final Capability scheduleYield(Task task) {
        Capability cap = this;
        boolean didGcLast = false;
        if (!cap.shouldYield(task,false) &&
            (!cap.emptyRunQueue() ||
             !cap.emptyInbox() ||
             schedulerState.compare(SCHED_INTERRUPTING) >= 0)) {
            return cap;
        }

        do {
            YieldResult result = cap.yield(task, !didGcLast);
            didGcLast = result.didGcLast;
            cap = result.cap;
        } while (cap.shouldYield(task, didGcLast));
        return cap;
    }

    public static class YieldResult {
        public final Capability cap;
        public final boolean didGcLast;

        public YieldResult(final Capability cap, final boolean didGcLast) {
            this.cap = cap;
            this.didGcLast = didGcLast;
        }
    }

    public final YieldResult yield(Task task, final boolean gcAllowed) {
        Capability cap = this;
        boolean didGcLast = false;

        if ((pendingSync == SyncType.SYNC_GC_PAR) && gcAllowed) {
            cap.gcWorkerThread();
            if (task.cap == cap) {
                didGcLast = true;
            }
        }

        task.wakeup = false;
        Lock l = cap.lock;
        boolean unlocked = false;
        l.lock();
        try {
            if (task.isWorker()) {
                cap.enqueueWorker();
            }
            cap.release_(false);
            if (task.isWorker() || task.isBound()) {
                l.unlock();
                unlocked = true;
                cap = task.waitForWorkerCapability();
            } else {
                cap.newReturningTask(task);
                l.unlock();
                unlocked = true;
                cap = task.waitForReturnCapability();
            }
        } finally {
            if (!unlocked) {
                l.unlock();
            }
        }
        return new YieldResult(cap,didGcLast);
    }

    public final boolean shouldYield(Task task, boolean didGcLast) {
        // TODO: Refine this condition
        return ((pendingSync != SyncType.SYNC_NONE && !didGcLast) ||
                !returningTasks.isEmpty() ||
                (!emptyRunQueue() && (task.incall.tso == null
                                          ? peekRunQueue().bound != null
                                          : peekRunQueue().bound != task.incall)));
    }

    public final TSO peekRunQueue() {
        return runQueue.peek();
    }

    public final Capability detectDeadlock(Task task) {
        Capability cap = this;
        boolean threaded = RtsFlags.ModeFlags.threaded;
        if (cap.emptyThreadQueues()) {
            if (threaded) {
                if (recentActivity != Inactive) return cap;
            }
            //scheduleDoGC
            if (!cap.emptyRunQueue()) return cap;
            // deal with user signals

            if (!threaded) {
                if (task.incall.tso != null) {
                    switch (task.incall.tso.whyBlocked) {
                        case BlockedOnSTM:
                        case BlockedOnBlackHole:
                        case BlockedOnMsgThrowTo:
                        case BlockedOnMVar:
                        case BlockedOnMVarRead:
                            cap.throwToSingleThreaded(task.incall.tso,
                                                      Capability.nonTermination_closure);
                            return cap;
                        default:
                            barf("deadlock: main thread blocked in a strange way");
                    }
                }
                return cap;
            }
        }
        return cap;
    }

    /* Sparks */

    public final void findWork() {
        if (emptyRunQueue()) activateSpark();
    }

    public final void activateSpark() {
        if (anySparks() && !cap.disabled) {
            TSO tso = createSparkThread();
            if (RtsFlags.DebugFlag.scheduler) {
                debugBelch("{Scheduler} Creating a Spark TSO[%d].", tso.id);
            }
        }
    }

    public static boolean anySparks() {
        for (Capability cap:capabilities) {
            if (!cap.emptySparkPool()) {
                return true;
            }
        }
        return false;
    }

    public final TSO createSparkThread(Capability cap) {
        TSO tso = Rts.createIOThread(cap, runSparks_closure);
        appendToRunQueue(tso);
        return tso;
    }

    public final boolean newSpark(Closure p) {
        if (p.getEvaluated() == null) {
            if (sparks.offerFirst(p)) {
                sparkStats.created++;
            } else {
                sparkStats.overflowed++;
            }
        } else {
            sparkStats.dud++;
        }
        return true;
    }

    public final Closure findSpark() {
        if (!emptyRunQueue() || !cap.returningTasks.isEmpty()) {
            return null;
        }
        boolean retry;
        do {
            retry = false;
            Closure spark = sparks.pollLast();
            while (spark != null && spark.getEvaluated() != null) {
                sparkStats.fizzled++;
                spark = sparks.pollLast();
            }
            if (spark != null) {
                sparkStats.converted++;
                return spark;
            }
            if (!emptySparkPool()) {
                retry = true;
            }
            if (capabilities.size() == 1) return null;
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("{Scheduler} Capability[%d]: Trying to steal work from other Capabilities.");
            }
            for (Capability robbed:capabilities) {
                if (cap == robbed || robbed.emptySparkPool()) continue;
                spark = robbed.sparks.pollLast();
                while (spark != null && spark.getEvaluated() != null) {
                    sparkStats.fizzled++;
                    spark = robbed.sparks.pollLast();
                }
                if (spark == null && !robbed.emptySparkPool()) {
                    retry = true;
                }
                if (spark != null) {
                    sparkStats.converted++;
                    return spark;
                }
            }
        } while(retry);
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("{Scheduler} No Sparks stolen.");
        }
    }

    public final void pushWork(Task task) {
        // int spareThreads = cap.emptyRunQueue()? 0 : cap.runQueueSize() - 1;
        // if (!RtsFlags.ParFlags.migrate) {
        //     spareThreads = 0;
        // }
        // int wantedCaps = cap.sparkPoolSize() + spareThreads;
        // if (wantedCaps == 0) return;
        // ArrayList<Capability> freeCapabilities = new ArrayList<Capability>(capabilities.size() / 4);
        // Capability first = null;
        // for (Iterator<Capability> it = capabilities.iterator();
        //      it.hasNext() && freeCapabilities.size() < wantedCaps;)
        //     Capability cap = it.next();
        //     if (first == null) first = cap;
        //     if (cap == first) break;
        //     if (cap != this && !cap.disabled && cap.tryGrab(task)) {
        //         if (!cap.emptyRunQueue()
        //             || !cap.returningTasks.isEmpty()
        //             || !cap.emptyInbox()) {
        //             cap.release();
        //         } else {
        //             freeCapabilities.add(cap);
        //         }
        //     }
        // }
        // int nFreeCapabilities = freeCapabilities.size();
        // if (nFreeCapabilities > 0) {
        //     int runQueueSize = cap.runQueueSize();
        //     if (RtsFlags.DebugFlags.scheduler) {
        //         debugBelch("Capability[%d]: %d TSOs, %d and %d sparks, and %d free capabilities, sharing..."
        //                    ,this.no
        //                    , cap.runQueueSize(),
        //                    , cap.sparkPoolSize()
        //                    nFreeCapabilities);
        //     }
        //     int keepThreads = (runQueueSize + nFreeCapabilities)
        //                     / (nFreeCapabilities + 1);
        //     int n = 
        //     if (!emptyRunQueue()) {
        //         // TODO: Object pool this?
        //         Deque<TSO> newRunQueue = new ArrayDeque<TSO>(1);
        //         newRunQueue.offer(popRunQueue());
        //         TSO t = peekRunQueue();
        //         TSO next = null;
        //         for (; t != null; t = next) {
        //             next = popRunQueue();
        //             if (t.bound == task.incall
        //                 || t.isFlagLocked()) {
        //                 newRunQueue.offer(t);
        //             } else if (i == nFreeCapabilities) {
        //                 i = 0;;
        //                 newRunQueue.offer(t);
        //             } else {
        //                 Capability freeCap = freeCapabilities.get(i);
        //                 freeCap.appendToRunQueue(t);
        //                 if (t.bound != null) {
        //                     t.bound.task().cap = freeCap;
        //                 }
        //                 t.cap = freeCap;
        //                 i++;
        //             }
        //         }
        //         runQueue = newRunQueue;
        //     }

        //     for (Capability freeCap: freeCapabilities) {
        //         task.cap = freeCap;
        //         freeCap.releaseAndWakeup();
        //     }
        // }
        // task.cap = this;
    }

    public final void startWorkerTask() {
        Task task = new Task(true);
        task.initialize();
        Lock l = task.lock;
        l.lock();
        try {
            task.cap = this;
            runningTask = task;
            WorkerThread thread = new WorkerThread(task);
            thread.setTask();
            thread.start();
        } finally {
            l.unlock();
        }
    }

    public final boolean emptyRunQueue() {
        return runQueue.isEmpty();
    }

    public final int runQueueSize() {
        return runQueue.size();
    }

    public final boolean emptyThreadQueues() {
        return emptyRunQueue();
    }

    public final void threadPaused(TSO tso) {
        maybePerformBlockedException(tso);
        UpdateInfo ui = tso.updateInfoStack.markBackwardsFrom(this, tso);
        if (ui != null) {
            suspendComputation(tso, ui);
        }
    }

    public final boolean maybePerformBlockedException(TSO tso) {
        Queue<MessageThrowTo> blockedExceptions = tso.blockedExceptions;
        boolean noBlockedExceptions = blockedExceptions.isEmpty();
        if (tso.whatNext == ThreadComplete) {
            if (noBlockedExceptions) {
                return false;
            } else {
                awakenBlockedExceptionQueue(tso);
                return true;
            }
        } else {

            if (!noBlockedExceptions && tso.hasFlag(TSO_BLOCKEX) && RtsFlags.DebugFlags.scheduler) {
                debugBelch("cap %d: throwTo: thread %d has blocked exceptions but is inside block", this.no, tso.id);
            }

            if (!noBlockedExceptions && (!tso.hasFlag(TSO_BLOCKEX) || (tso.hasFlag(TSO_INTERRUPTIBLE) && tso.interruptible()))) {
                do {
                    TSO source;
                    MessageThrowTo msg = blockedExceptions.peek();
                    if (msg == null) return false;
                    synchronized (msg) {
                        blockedExceptions.poll();
                        if (!msg.isValid()) {
                            continue;
                        }
                        throwToSingleThreaded(msg.target, msg.exception);
                        source = msg.source;
                        msg.done();
                    }
                    barf("TODO: park/unpark here");
                    // tryWakeupThread(source);
                } while (false);
                return true;
            }
            return false;
        }
    }

    public final void awakenBlockedExceptionQueue(TSO tso) {
        synchronized (tso.blockedExceptions) {
            for (MessageThrowTo msg: tso.blockedExceptions) {
                synchronized(msg) {
                    if (msg.isValid()) {
                        TSO source = msg.source;
                        msg.done();
                        source.interrupt();
                    }
                }
            }
            tso.blockedExceptions.clear();
        }
    }

    public final void tryWakeupThread(TSO tso) {
        tso.interrupt();
    }

    public final void sendMessage(Capability target, Message msg) {
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

    public final void release_(boolean alwaysWakeup) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("{Scheduler} Releasing Capability[%d].", this.no);
        }

        if (RtsFlags.ModeFlags.threaded) {
            Task task = runningTask;
            assert assertPartialCapabilityInvariants(task);
            assert assertReturningTasks(task);
            runningTask = null;
            Task workerTask = returningTasks.peek();
            if (workerTask != null) {
                giveToTask(workerTask);
                return;
            }

            if (pendingSync != SyncType.SYNC_NONE
             && pendingSync != SyncType.SYNC_GC_PAR) {
               lastFreeCapability = this;
               if (RtsFlags.DebugFlags.scheduler) {
                   debugBelch("sync pending, set capability %d free", this.no);
               }
               return;
            }

            if (!emptyRunQueue()) {
                TSO nextTSO = peekRunQueue();
                if (nextTSO.bound != null) {
                    task = nextTSO.bound.task();
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("Giving Capability[%d] to Task[%d].", this.no, task.id);
                    }
                    giveToTask(task);
                    return;
                }
            }

            if (spareWorkers.isEmpty()) {
                if (// schedulerState.compare(SCHED_SHUTTING_DOWN) < 0 ||
                    // TODO: Verify that this condition is ok
                    !emptyRunQueue()) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("Starting new Worker Task on Capability[%d]", this.no);
                    }
                    startWorkerTask();
                    return;
                }
            }

            if (alwaysWakeup || !emptyRunQueue() || !emptyInbox()
                || (!disabled && !emptySparkPool()) || globalWorkToDo()) {
                task = spareWorkers.peek();
                if (task != null) {
                    /* TODO: Verify that the worker task pops itself from
                             queue. */
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("Giving Capability[%d] to Worker Task[%d].", this.no, task.id);
                    }
                    giveToTask(task);
                    return;
                }
            }

            lastFreeCapability = this;
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("Freeing Capability[%d].", this.no);
            }
        }
    }

    public final void release() {
        if (RtsFlags.ModeFlags.threaded) {
            lock.lock();
            try {
                release_(false);
            } finally {
                lock.unlock();
            }
        }
    }

    public final void releaseAndWakeup() {
        if (RtsFlags.ModeFlags.threaded) {
            lock.lock();
            try {
                release_(true);
            } finally {
                lock.unlock();
            }
        }
    }

    public final boolean emptyInbox() {
        return inbox.isEmpty();
    }

    public final boolean emptySparkPool() {
        return sparks.isEmpty();
    }

    public final boolean globalWorkToDo() {
        return (schedulerState.compare(SCHED_INTERRUPTING) >= 0)
            || recentActivity == Inactive;
    }

    public final void interrupt() {
        stop();
        interrupt = true;
    }

    public final void contextSwitch() {
        stop();
        contextSwitch = true;
    }

    public final void stop() {
        // Find a method to force the stopping of a thread;
    }

    public final void throwToSingleThreaded(TSO tso, Closure exception) {
        throwToSingleThreaded__(tso, exception, false, null);
    }

    public final void throwToSingleThreaded_(TSO tso, Closure exception,
                                       boolean stopAtAtomically) {
        throwToSingleThreaded__(tso, exception, stopAtAtomically, null);
    }

    public final void throwToSingleThreaded__(TSO tso, Closure exception,
                                        boolean stopAtAtomically,
                                        UpdateInfo stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }

        removeFromQueues(tso);
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public final void suspendComputation(TSO tso, UpdateInfo stopHere) {
        throwToSingleThreaded__(tso, null, false, stopHere);
    }

    public final void removeFromQueues(TSO tso) {
        switch (tso.whyBlocked) {
            case NotBlocked:
            case ThreadMigrating:
                return;
            case BlockedOnSTM:
                /* Check for zombie transactions */
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
        // Not necessary - it's already in the run queue!
        // appendToRunQueue(tso);
    }

    public final void raiseAsync(TSO tso, Closure exception, boolean stopAtAtomically, UpdateInfo stopHere) {

        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap: %d message: raising exception in thread %d.", tso.id);
        }
        assert tso.whatNext != ThreadComplete && tso.whatNext != ThreadKilled;
        assert tso.cap == this;

        if (tso.whyBlocked != NotBlocked) {
            tso.whyBlocked = NotBlocked;
        }
        throw new EtaAsyncException(exception, stopAtAtomically, stopHere);
    }

    public final boolean messageBlackHole(MessageBlackHole msg) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap: %d message: thread %d blocking on blackhole " + msg.bh
                       , no, msg.tso.id, msg.bh);
        }

        Thunk bh = msg.bh;
        do {
            Closure p = bh.indirectee;
            if (p instanceof TSO) {
                TSO owner = (TSO) p;
                if (Capability.nCapabilities > 1) {
                    synchronized (owner) {
                        if (bh.indirectee instanceof TSO) {
                            BlockingQueue bq = new BlockingQueue(owner, msg);
                            owner.blockingQueues.offer(bq);
                            bh.indirectee = bq;
                            if (RtsFlags.DebugFlags.scheduler) {
                                debugBelch("(synchronized) cap %d: thread %d blocked on thread %d",
                                           cap.no, msg.tso.id, id);
                            }
                        } else {
                            continue;
                        }
                    }
                } else {
                    /* NOTE: Keep this in sync with above */
                    BlockingQueue bq = new BlockingQueue(owner, msg);
                    owner.blockingQueues.offer(bq);
                    bh.indirectee = bq;
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("cap %d: thread %d blocked on thread %d",
                                   cap.no, msg.tso.id, id);
                    }
                }
                return true;
            } else if (p instanceof BlockingQueue) {
                BlockingQueue bq = (BlockingQueue) p;
                assert bq.bh == bh;
                if (Capability.nCapabilities > 1) {
                    synchronized (bq) {
                        if (bh.indirectee instanceof BlockingQueue) {
                            messages.offer(msg);
                            if (RtsFlags.DebugFlags.scheduler) {
                                debugBelch("cap %d: thread %d blocked on thread %d",
                                           msg.tso.id, owner.id);
                            }
                        } else {
                            continue;
                        }
                    }
                } else {
                    /* NOTE: Keep this in sync with above */
                    messages.offer(msg);
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("cap %d: thread %d blocked on thread %d",
                                   msg.tso.id, owner.id);
                    }
                }
                return true
            } else return false;
        } while (false);
    }

    public final void checkBlockingQueues(TSO tso) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap %d: collision occured; checking blockign queues for thread %d", no, tso.id);
        }
        for (BlockingQueue bq: tso.blockingQueues) {
            Closure p = bq.bh;
            if (p.indirectee == null || p.indirectee != bq) {
                wakeBlockingQueue(bq);
            }
        }
    }

    public final void wakeBlockingQueue(BlockingQueue blockingQueue) {
        synchronized (blockingQueue) {
            Thunk bh = blockingQueue.bh;
            for (MessageBlackHole msg: blockingQueue) {
                TSO tso = msg.tso;
                /* Ensure that the tso in the list is still blocked on the *same*
                   thunk. This is a safety for doing unpark() in case we use
                   unpark() for other purposes. */
                if (tso.whyBlocked == BlockedOnBlackHole &&
                    bh == (((MessageBlackHole) tso.blockInfo).bh)) {
                    LockSupport.unpark(msg.thread.get());
                }
            }
            blockingQueue.clear();
        }
    }

    public final SchedulerStatus getSchedStatus() {
        return runningTask.incall.returnStatus;
    }

    public final static Capability getFreeRunningCapability() {
        for (Capability cap: capabilities) {
            if (cap.emptyRunQueue() && cap.runningTask != null && !cap.inStg) {
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("Capability[%d] is free to run a new forked thread.", cap.no);
                }
                return cap;
            }
        }
        return lastFreeCapability;
    }

    public final static Capability getFreeCapability() {
        if (lastFreeCapability.runningTask != null) {
            for (Capability cap: capabilities) {
                if (cap.runningTask == null) {
                    return cap;
                }
            }
        }
        return lastFreeCapability;
    }

    public final void newReturningTask(Task task) {
        returningTasks.add(task);
    }

    public final void popReturningTask() {
        returningTasks.poll();
    }

    public final void giveToTask(Task task) {
        assert cap.lock.isHeldByCurrentThread();
        assert task.cap == cap;
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("{Scheduler} Passing Capability[%d] to %s Task[%d].",
                       cap.no, (task.incall.tso != null)? "Bound" : "Worker", task.id);
        }
        Lock l = task.lock;
        l.lock();
        try {
            if (!task.wakeup) {
                task.wakeup = true;
                task.condition.signalAll();
            }
        } finally {
            l.unlock();
        }
    }

    public final Task suspendThread(boolean interruptible) {
        Task task = runningTask;
        TSO tso = context.currentTSO;
        tso.whatNext = ThreadRun;
        threadPaused(tso);
        if (interruptible) {
            tso.whyBlocked = BlockedOnJavaCall_Interruptible;
        } else {
            tso.whyBlocked = BlockedOnJavaCall;
        }
        Task.InCall incall = task.incall;
        incall.suspendedTso = tso;
        incall.suspendedCap = this;
        context.currentTSO = null;
        lock.lock();
        try {
            suspendTask(task);
            inStg = false;
            release_(false);
        } finally {
            lock.unlock();
        }
        return task;
    }

    public final static Capability resumeThread(Task task) {
        Task.InCall incall = task.incall;
        Capability cap = incall.suspendedCap;
        task.cap = cap;
        cap = task.waitForCapability(cap);
        cap.recoverSuspendedTask(task);
        TSO tso = incall.suspendedTso;
        incall.suspendedTso = null;
        incall.suspendedCap = null;
        // remove the tso _link
        tso.whyBlocked = NotBlocked;
        if (tso.hasFlag(TSO_BLOCKEX)) {
            if (!tso.blockedExceptions.isEmpty()) {
                cap.maybePerformBlockedException(tso);
            }
        }
        cap.context.currentTSO = tso;
        cap.inStg = true;
        return cap;
    }

    public final void suspendTask(Task task) {
        // TODO: Should the incall be removed from any existing data structures?
        suspendedJavaCalls.push(task.incall);
    }

    public final void recoverSuspendedTask(Task task) {
        suspendedJavaCalls.remove(task.incall);
    }

    public final void scheduleWorker(Task task) {
        Capability cap = schedule(task);
        Lock l = cap.lock;
        l.lock();
        try {
            cap.release_(false);
            task.workerTaskStop();
        } finally {
            l.unlock();
        }
    }
    public final boolean singletonRunQueue() {
        return (runQueue.size() == 1);
    }

    public final boolean tryGrab(Task task) {
        if (runningTask != null) return false;
        lock.lock();
        boolean unlocked = false;
        try {
            if (runningTask != null) {
                lock.unlock();
                unlocked = true;
                return false;
            } else {
                task.cap = this;
                runningTask = task;
            }
        } finally {
            if (!unlocked) {
                lock.unlock();
            }
        }
        return true;
    }

    public final void enqueueWorker() {
        Task task = runningTask;
        boolean taken = spareWorkers.offer(task);
        if (!taken) {
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("%d spare workes already, exiting", spareWorkers.size());
            }
            release_(false);
            task.workerTaskStop();
            lock.unlock();
            Task.shutdownThread();
        }
    }

    public final void gcWorkerThread() {
        // TODO: Implement
    }

    public static void contextSwitchAll() {
        for (Capability c: capabilities) {
            c.contextSwitch();
        }
    }

    public static void interruptAll() {
        for (Capability c: capabilities) {
            c.interrupt();
        }
    }

    public final boolean handleThreadFinished(Task task, TSO t) {
        awakenBlockedExceptionQueue(t);

        if (t.bound != null) {
            if (t.bound != task.incall) {
                    barf("finished bound thread that isn't mine");
                } else {
                    appendToRunQueue(t);
                    return false;
                }
            }

            assert task.incall.tso == t;

            if (t.whatNext == ThreadComplete) {
                StgEnter enterFrame = (StgEnter) task.incall.tso.stack.peek();
                task.incall.ret = enterFrame.closure;
                task.incall.returnStatus = Success;
            } else {
                if (task.incall.ret != null) {
                    task.incall.ret = null;
                }
                if (schedulerState.compare(SCHED_INTERRUPTING) >= 0) {
                    if (false /* HeapOverflow */) {
                        task.incall.returnStatus = HeapExhausted;
                    } else{
                        task.incall.returnStatus = Interrupted;
                    }
                } else {
                    task.incall.returnStatus = Killed;
                }
            }
            t.bound = null;
            task.incall.tso = null;
            return true;
        }

        return false;
    }

    public final boolean handleYield(TSO t, WhatNext prevWhatNext) {
        if (!contextSwitch && t.whatNext != prevWhatNext) {
            return true;
        }

        if (contextSwitch) {
            contextSwitch = false;
            appendToRunQueue(t);
        } else {
            pushOnRunQueue(t);
        }

        return false;
    }

    public final StgResult ioManagerStart() {
        return new StgResult(this, null);
        /* TODO: Implement IO Manager, Rts.evalIO(this, null /* base_GHCziConcziIO_ensureIOManagerIsRunning_closure */
    }

    public final void promoteInRunQueue(TSO tso) {
        removeFromRunQueue(tso);
        pushOnRunQueue(tso);
    }

    public final void removeFromRunQueue(TSO tso) {
        runQueue.remove(tso);
    }

    public final MessageThrowTo throwTo(TSO source, TSO target, Closure exception) {
        MessageThrowTo msg = new MessageThrowTo(source, target, exception);
        boolean success = throwToMsg(msg);
        if (success) {
            msg.unlock();
            return null;
        } else {
            return msg;
        }
    }

    public final boolean throwToMsg(MessageThrowTo msg) {
        TSO target = msg.target;
        if (target.whatNext == ThreadComplete
            || target.whatNext == ThreadKilled) {
            return true;
        }
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("Capability[%d](throwTo) From TSO %d to TSO %d.", msg.source.id, msg.target.id);
        }
        barf("throwTo: Unimplemented");
        // synchronized (target) {
        //     WhyBlocked status = target.whyBlocked;
        //     switch (status) {
        //         case NotBlocked:
        //             synchronized (msg) {
        //                 target.blockedExceptions.offer(msg);
        //                 msg.wait();
        //             }
        //             return false;
        //         }
        //         break;


        // }
    }

    /* STM Operations */

    public final InvariantCheck getNewInvariantCheck(AtomicInvariant invariant) {
        return new InvariantCheck(invariant);
    }

    public final boolean stmCommitNestedTransaction(TransactionRecord trec) {
        STM.lock(trec);
        TransactionRecord et = trec.enclosingTrec;
        boolean result = validateAndAcquireOwnership(trec, !STM.configUseReadPhase, true);
        if (result) {
            if (STM.configUseReadPhase) {
                result = trec.checkReadOnly();
            }
            if (result) {
                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                loop:
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TRecEntry e: chunk.entries) {
                        TVar s = e.tvar;
                        if (e.isUpdate()) {
                            s.unlock(trec, e.expectedValue, false);
                        }
                        mergeUpdateInto(et, s, e.expectedValue, e.newValue);
                    }
                }
            } else {
                revertOwnership(trec, false);
            }
        }
        STM.unlock(trec);
        freeTRecHeader(trec);
        return result;
    }

    public final boolean validateAndAcquireOwnership(TransactionRecord trec, boolean acquireAll, boolean retainOwnership) {
        if (STM.shake()) {
            return false;
        } else {
            boolean result = trec.state != TREC_CONDEMNED;
            if (result) {
                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                loop:
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TRecEntry e: chunk.entries) {
                        // Traversal
                        TVar s = e.tvar;
                        if (acquireAll || e.isUpdate()) {
                            if (!s.condLock(trec, e.expectedValue)) {
                                result = false;
                                break loop;
                            }
                        } else {
                            if (RtsFlags.STM.fineGrained) {
                                if (s.currentValue != e.expectedValue) {
                                    result = false;
                                    break loop;
                                }
                                e.numUpdates = s.numUpdates;
                                if (s.currentValue != e. expectedValue) {
                                    result = false;
                                    break loop;
                                }
                            }
                        }
                    }
                }
            }

            if (!result || !retainOwnership) {
                revertOwnership(trec, acquireAll);
            }

            return result;
        }
    }

    public final void revertOwnership(TransactionRecord trec, boolean revertAll) {
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    if (revertAll || e.isUpdate()) {
                        TVar s = e.tvar;
                        if (s.isLocked(trec)) {
                            s.unlock(trec, e.expectedValue, true);
                        }
                    }

                }
            }
        }
    }

    public final Thunk newCAF(CAF caf) {
        Thunk bh = lockCAF(caf);
        if (bh == null) return null;
        if (Thunk.shouldKeepCAFs()) {
            Thunk.revertibleCAFList.offer(caf);
            // Thunk.dynamicCAFList.offer(caf); TODO: Handle when Eta REPL is working
        } else {
            /* TODO: Save the info tables during debugging */
        }
        return bh;
    }

    public final Thunk lockCAF(CAF caf) {
        TSO tso = context.currentTSO;
        if (Capability.capabilities.size() > 1) {
            synchronized (caf) {
                if (caf.indirectee == null) {
                    caf.indirectee = tso;
                } else {
                    return null;
                }
            }
        } else {
            caf.indirectee = tso;
        }
        return caf;
    }

    public static void freeCapabilities() {
        for (Capability c: capabilities) {
            if (c != mainCapability) {
                c.free();
            }
        }
    }

    public final void free() {
        sparks.clear();
    }

    public static void shutdownCapabilities(Task task, boolean safe) {
        for (Capability c: capabilities) {
            c.shutdown(task, safe);
        }
    }

    public final void shutdown(Task task, boolean safe) {
        task.cap = this;
        while (true) {
            lock.lock();
            boolean unlocked = false;
            try {
                if (runningTask != null) {
                    lock.unlock();
                    unlocked = true;
                    Thread.yield();
                    continue;
                }
                runningTask = task;
                if (!spareWorkers.isEmpty()) {
                    Task prev = null;
                    Iterator<Task> it = spareWorkers.iterator();
                    while (it.hasNext()) {
                        Task t = it.next();
                        if (!t.isAlive()) {
                            it.remove();
                        }
                    }
                }

                if (!emptyRunQueue() || !spareWorkers.isEmpty()) {
                    release_(false);
                    lock.unlock();
                    unlocked = true;
                    Thread.yield();
                    continue;
                }

                if (!suspendedJavaCalls.isEmpty() && safe) {
                    runningTask = null;
                    lock.unlock();
                    unlocked = true;
                    RtsIO.ioManagerDie();
                    Thread.yield();
                    continue;
                }
            } finally {
                if (!unlocked) {
                    lock.unlock();
                }
            }
            break;
        }
    }

    public final long sparkPoolSize() {
        return sparks.size();
    }

    public final boolean assertFullCapabilityInvariants(Task task) {
        assert runningTask != null && runningTask == task;
        assert task.cap == this;
        return assertPartialCapabilityInvariants(task);
    }

    public final boolean assertPartialCapabilityInvariants(Task task) {
        assert Task.myTask() == task;
        return task.assertTaskId();
    }
}
