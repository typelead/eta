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

public final class Capability {
    public static final int MAX_SPARE_WORKERS = 6;
    public static Capability lastFreeCapability;
    public static List<Capability> capabilities = new Concurrent<Capability>();
    public static ReferenceQueue<Capability> capabilities = new Concurrent<Capability>();
    private static ThreadLocal<Capability> myCapability = new ThreadLocal<Capability>();

    public static void init() {
        ensureCapabilities(RtsFlags.ParFlags.nNodes);
        lastFreeCapability = capabilities.get(0);
    }

    public static synchronized void ensureCapabilities(int n) {
        int i = capabilities.size();
        while (i < n) {
            capabilities.add(new Capability(i++));
        }
    }

    public static void setNumCapabilities(int n) {
        ensureCapabilities(n);
        /* TODO: Do some form of disabling capabilities if n < capabilities.size(); */
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
    public volatile boolean disabled;
    public Deque<TSO> runQueue = new ConcurrentLinkedDeque<TSO>();
    public Deque<InCall> suspendedJavaCalls = new ArrayDeque<InCall>();
    public Queue<Task> spareWorkers = new ArrayBlockingQueue<Task>(MAX_SPARE_WORKERS);
    public Deque<Task> returningTasks = new ArrayDeque<Task>();
    public Deque<Message> inbox = new ConcurrentLinkedDeque<Message>();
    public Map<WeakReference<Closure>, WeakPtr> weakPtrMap = new ArrayList<Weak>();
    public ReferenceQueue<Closure> refQueue = new ReferenceQueue<Closure>();

    public Capability(int i) {
        this.no = i;
    }

    public final Closure schedule(Task task) {
        Closure result = null;
        TSO outer      = null;

        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("Capability[%d]: schedule()", cap.no);
        }
        while (true) {
            if (cap) {
                /* Re-entering the RTS, a fresh TSO was generated. */
                inStg = false;
                outer = context.currentTSO;
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
                    if (!task.isBound() && emptyRunQueue()) {
                        return cap;
                    }
                    break;
                default:
                    barf("FATAL ERROR: Invalid scheduler state: " + schedulerState);
            }
            findWork();
            /* TODO: The following still need to be implemented:
                     - Deadlock detection. Be able to detect <<loop>>.
                     - Killing TSO on shutdown.
                     - Migrate threads when disabled?
            */

            // todo: yielding is causing unnecessary work for all the capabilities.
            //       disabling until we find a need for it.
            // cap = cap.scheduleyield(task);
            if (emptyRunQueue()) continue;

            TSO t = popRunQueue();
            Task.InCall bound = t.bound;
            if (bound != null) {
                Task boundTask = bound.task();
                if (boundTask != task) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} TSO[%d] is bound to Task[%d], but current Task[%d].", t.id, boundTask.id, task.id);
                    }
                    pushOnRunQueue(t);
                    continue;
                }
            } else {
                if (task.incall.tso != null) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("{Scheduler} Current Task[%d] cannot run TSO[%d].", task.id, t.id);
                    }
                    pushOnRunQueue(t);
                    continue;
                }
            }

            context.reset(cap, t);
            inStg = true;

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
                    }
                    break;
                case ThreadInterpret:
                    interpretBCO(cap);
                    break;
                default:
                    barf("{Scheduler} Invalid whatNext field for TSO[%d].", t.id);
            }

            inStg = false;
            context.currentTSO = null;

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
                return;
            }
        }
        return;
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
        if (!cap.shouldYield(task,false) &&
            (!cap.emptyRunQueue() ||
             !cap.emptyInbox() ||
             schedulerState.compare(SCHED_INTERRUPTING) >= 0)) {
            return cap;
        }

        do {
            cap = cap.yield(task);
        } while (cap.shouldYield(task));
        return cap;
    }

    public final Capability yield(Task task) {
        Capability cap = this;
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
        return cap;
    }

    public final boolean shouldYield(Task task, boolean didGcLast) {
        // TODO: Refine this condition
        return (!returningTasks.isEmpty() ||
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
        if (emptyRunQueue()) tryStealGlobalRunQueue();
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
                    MessageThrowTo msg = tso.blockedExceptions.peek();
                    if (msg == null) return false;
                    msg.lock();
                    tso.blockedExceptions.poll();
                    if (!msg.isValid()) {
                        msg.unlock();
                        continue;
                    }
                    TSO source = msg.source;
                    msg.done();
                    tryWakeupThread(source);
                    throwToSingleThreaded(msg.target, msg.exception);
                    break;
                } while (true);
                return true;
            }
            return false;
        }
    }

    public final void awakenBlockedExceptionQueue(TSO tso) {
        MessageThrowTo msg;

        while ((msg = tso.blockedExceptions.poll()) != null) {
            msg.lock();
            if (msg.isValid()) {
                TSO source = msg.source;
                msg.done();
                tryWakeupThread(source);
            } else {
                msg.unlock();
            }
        }
    }

    public final void tryWakeupThread(TSO tso) {
        if (tso.cap != cap) {
            sendMessage(tso.cap, new MessageWakeup(tso));
        } else {
            boolean blocked = true;
            switch (tso.whyBlocked) {
                case BlockedOnMVar:
                case BlockedOnMVarRead:
                    /* TODO: fix this */
                    blocked = true;
                    break;
                case BlockedOnMsgThrowTo:
                    MessageThrowTo msg = (MessageThrowTo) tso.blockInfo;
                    if (msg.isValid()) {
                        return;
                    }
                case BlockedOnBlackHole:
                case BlockedOnSTM:
                    blocked = true;
                    break;
                case ThreadMigrating:
                    blocked = false;
                    break;
                default:
                    return;

            }
            tso.whyBlocked = NotBlocked;
            if (!blocked) {
                appendToRunQueue(tso);
            }
        }
    }

    public final void sendMessage(Capability target, Message msg) {
        Lock lock = target.lock;
        lock.lock();
        try {
            target.inbox.offer(msg);
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
        Task task = runningTask;
        assert assertPartialCapabilityInvariants(task);
        assert assertReturningTasks(task);
        runningTask = null;
        Task workerTask = returningTasks.peek();
        if (workerTask != null) {
            giveToTask(workerTask);
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
            if (schedulerState.compare(SCHED_SHUTTING_DOWN) < 0 || !emptyRunQueue()) {
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("Starting new Worker Task on Capability[%d].", this.no);
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
        if (runningTask != null) {
            runningTask.interrupt();
        }
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
            if (p instanceof WhiteHole) {
                return false;
            } else if (p instanceof TSO) {
                TSO owner = (TSO) p;
                if (owner.cap != this) {
                    sendMessage(owner.cap, msg);
                    return true;
                }
                BlockingQueue bq = new BlockingQueue(owner, msg);
                owner.blockingQueues.offer(bq);
                bh.setIndirection(bq);
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("cap %d: thread %d blocked on thread %d",
                                cap.no, msg.tso.id, id);
                }
                return true;
            } else if (p instanceof BlockingQueue) {
                BlockingQueue bq = (BlockingQueue) p;
                assert bq.bh == bh;
                TSO owner = bq.owner;
                assert owner != null;
                if (owner.cap != this) {
                    sendMessage(owner.cap, msg);
                    return true;
                }
                messages.offer(msg);
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("cap %d: thread %d blocked on thread %d",
                                msg.tso.id, owner.id);
                }
                return true;
            } else return false;
        } while (true);
    }

    public final void checkBlockingQueues(TSO tso) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap %d: collision occured; checking blocking queues for thread %d", no, tso.id);
        }
        for (BlockingQueue bq: tso.blockingQueues) {
            Closure p = bq.bh;
            Closure ind = p.indirectee;
            /* TODO: Is this the correct condition? */
            if (ind == null || ind != bq) {
                wakeBlockingQueue(bq);
            }
        }
    }

    public final void wakeBlockingQueue(BlockingQueue blockingQueue) {
        for (MessageBlackHole msg: blockingQueue) {
            if (msg.isValid()) {
                tryWakeupThread(msg.tso);
            }
        }
        blockingQueue.clear();
    }

    public final SchedulerStatus getSchedStatus() {
        return runningTask.incall.returnStatus;
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

    public static void interruptAll() {
        for (Capability c: capabilities) {
            c.interrupt();
        }
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
        msg.lock();
        boolean success = throwToMsg(msg, false);
        if (success) {
            msg.unlock();
            return null;
        } else {
            return msg;
        }
    }

    public final boolean throwToMsg(MessageThrowTo msg, boolean wakeupSource) {
        do {
            TSO target = msg.target;
            assert target != null;
            if (target.whatNext == ThreadComplete
                || target.whatNext == ThreadKilled) {
                return true;
            }
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("Capability[%d](throwTo) From TSO %d to TSO %d.", msg.source.id, msg.target.id);
            }
            Capability targetCap = target.cap;
            if (target.cap != this) {
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("Capability[%d](throwTo) Sending a ThrowTo message to Capability[%d].", no, targetCap.no);
                }
                sendMessage(targetCap, msg);
                return false;
            }
            switch (target.whyBlocked) {
                case NotBlocked:
                    if (target.hasFlag(TSO_BLOCKEX)) {
                        blockedThrowTo(target, msg);
                        return false;
                    }
                    break;
                case BlockedOnMsgThrowTo:
                    MessageThrowTo msg2 = (MessageThrowTo) target.blockInfo;
                    if (msg2.id < msg.id) {
                        msg2.lock();
                    } else {
                        if (!msg2.tryLock()) {
                            sendMessage(targetCap, msg);
                            return false;
                        }
                    }
                    if (!msg2.isValid()) {
                        msg.unlock();
                        cap.tryWakeupThread(target);
                        continue;
                    }
                    if (target.hasFlag(TSO_BLOCKEX) && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                        msg.unlock();
                        blockedThrowTo(target, msg);
                        return false;
                    }
                    break;
               case BlockedOnMVar:
               case BlockedOnMVarRead:
                   /* TODO: Figure out MVar story */
                   barf("Unimplemented MVar");
                   break;
               case BlockedOnBlackHole:
                   if (target.hasFlags(TSO_BLOCKEX)) {
                       blockedThrowTo(target, msg);
                       return false;
                   }
                   assert target.blockInfo instanceof MessageBlackHole;
                   ((MessageBlackHole) target.blockInfo).invalidate();
                   break;
               case BlockedOnSTM:
                   target.lock();
                   if (target.whyBlocked != BlockedOnSTM) {
                       target.unlock();
                       continue;
                   } else {
                       if (target.hasFlag(TSO_BLOCKEX)
                           && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                           blockedThrowTo(target, msg);
                           target.unlock();
                           return false;
                       } else {
                           target.unlock();
                       }
                       break;
                   }
               case BlockedOnRead:
               case BlockedOnWrite:
               case BlockedOnDelay:
                   barf("Unimplemented IO manager");
                   break;
               case ThreadMigrating:
                   tryWakeupThread(target);
                   continue;
               default:
                   barf("Unimplemented throwTo()");
            }
            break;
        } while (true);
        if (wakeupSource) {
            TSO source = msg.source;
            msg.done();
            cap.tryWakeupThread(source);
        }
        raiseAsync(target, msg.exception, false, null);
        return true;
    }

    public final void blockedThrowTo(TSO target, MessageThrowTo msg) {
        assert target.cap == this;
        target.blockedExceptions.offer(target);
    }

    public final Thunk newCAF(CAF caf, TSO tso) {
        Thunk bh = lockCAF(caf, tso);
        if (bh == null) return null;
        if (Thunk.shouldKeepCAFs()) {
            Thunk.revertibleCAFList.offer(caf);
        }
        return bh;
    }

    public final Thunk lockCAF(CAF caf) {
        TSO tso = context.currentTSO;
        if (caf.tryLock()) {
            caf.setIndirection(tso);
            return caf;
        } else return null;
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

    public TSO tryStealGlobalRunQueue() {
        TSO tso = globalRunQueue.pollLast();
        if (tso != null) {
            migrateThread(tso, this);
        }
    }

    public void checkFinalizers() {
        if (!weakPtrList.isEmpty()) {
            Reference<?> ref;
            while ((ref = refQueue.poll()) != null) {

            }
        }
    }

    public void blockedLoop(boolean actuallyBlocked) {
        TSO tso = context.currentTSO;
        processInbox();
        if (actuallyBlocked) {
            /* When it's blocked, we can assumed that threadPaused() has
               already been run, hence, we only need to check blocked exceptions. */
            maybePerformedBlockedExceptions(tso);
        } else {
            threadPaused(tso);
        }

        /* Run finalizers for WeakPtrs and ByteArrays */
        checkFinalizers();
    }

    public void processInbox() {
        Message msg;
        while ((msg = inbox.poll()) != null) {
            msg.execute(cap);
        }
    }
}
