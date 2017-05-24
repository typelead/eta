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
import static eta.runtime.stg.StgTSO.*;
import static eta.runtime.stg.StgTSO.WhatNext.*;
import static eta.runtime.stg.StgTSO.WhyBlocked.*;
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
    public volatile boolean inHaskell;
    public int idle;
    public volatile boolean disabled;
    public Deque<StgTSO> runQueue = new ConcurrentLinkedDeque<StgTSO>();
    public Deque<InCall> suspendedJavaCalls = new ArrayDeque<InCall>();
    public volatile boolean contextSwitch;
    public volatile boolean interrupt;
    public Queue<Task> spareWorkers = new ArrayBlockingQueue<Task>(MAX_SPARE_WORKERS);
    public Deque<Task> returningTasks = new ArrayDeque<Task>();
    public Deque<Message> inbox = new ArrayDeque<Message>();
    public WSDeque<StgClosure> sparks; //= new WSDeque<StgClosure>();

    public SparkCounters sparkStats = new SparkCounters();
    public List<StgWeak> weakPtrList = new ArrayList<StgWeak>();

    /* STM-related data structures */
    public Stack<StgTRecChunk> freeTRecChunks = new Stack<StgTRecChunk>();
    public Queue<StgInvariantCheck> freeInvariantChecksQueue = new ArrayDeque<StgInvariantCheck>();
    public Queue<StgTRecHeader> freeTRecHeaders = new ArrayDeque<StgTRecHeader>();
    public long transactionTokens;

    public int ioManagerControlWrFd; /* TODO: Finish implementation of IO manager */

    public Capability(int i) {
        this.no = i;
    }

    public final Capability schedule(Task task) {
        Capability cap = this;
        boolean threaded = RtsFlags.ModeFlags.threaded;
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("Capability[%d]: schedule()", cap.no);
        }
        while (true) {
            if (cap.inHaskell) {
                errorBelch("schedule: re-entered unsafely.\n" +
                           "    Perhaps a 'foreign import unsafe' should be 'safe'?");
                stgExit(EXIT_FAILURE);
            }

            switch (schedulerState) {
                case SCHED_RUNNING:
                    break;
                case SCHED_INTERRUPTING:
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("SCHED_INTERRUPTING");
                    }
                    // cap = null;//TODO: scheduleDoGC(cap, task, false);
                case SCHED_SHUTTING_DOWN:
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("SCHED_SHUTTING_DOWN");
                    }
                    if (!task.isBound() && cap.emptyRunQueue()) {
                        return cap;
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
                // TODO: Yielding is causing unnecessary work for all the capabilities.
                //       Disabling until we find a need for it.
                // cap = cap.scheduleYield(task);
                if (cap.emptyRunQueue()) continue;
            }

            StgTSO t = cap.popRunQueue();
            if (threaded) {
                Task.InCall bound = t.bound;
                if (bound != null) {
                    if (bound.task() != task) {
                        if (RtsFlags.DebugFlags.scheduler) {
                            debugBelch("TSO[%d] is bound to another OS thread.", t.id);
                        }
                        cap.pushOnRunQueue(t);
                        continue;
                    }
                } else {
                    if (task.incall.tso != null) {
                        if (RtsFlags.DebugFlags.scheduler) {
                            debugBelch("This OS thread cannot run TSO[%d].", t.id);
                        }
                        cap.pushOnRunQueue(t);
                        continue;
                    }
                }
            }

            if (schedulerState.compare(SCHED_INTERRUPTING) >= 0 &&
                !(t.whatNext == ThreadComplete || t.whatNext == ThreadKilled)) {
                t.delete();
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

            boolean runThread = true;
            while (runThread) {
                runThread = false;

                StgContext context = cap.context;
                context.reset(cap, t);
                cap.interrupt = false;
                cap.inHaskell = true;
                cap.idle = 0;

                WhatNext prevWhatNext = t.whatNext;
                // errno

                switch (recentActivity) {
                case DoneGC:
                    //xchg(recent_activity, ACTIVITy_YES)
                    //if (prev == ACTIVITY_DONE_GC)
                    // startTimer
                    // stuff here
                    break;
                case Inactive:
                    break;
                default:
                    recentActivity = Yes;
                }

                ReturnCode ret = HeapOverflow;
                switch (prevWhatNext) {
                case ThreadKilled:
                case ThreadComplete:
                    ret = ThreadFinished;
                    break;
                case ThreadRunGHC:
                    /* Update the context state */
                    context.myCapability = cap;

                    /* Reload is used for synchronizing the Java method call
                       stack with the actual stack of the TSO. Normally, it
                       isn't done. */
                    boolean reload = false;
                    do {
                        reload = false;
                        /* Rewind sp to the bottom of the stack */
                        ListIterator<StackFrame> sp = t.sp;
                        while (sp.hasPrevious()) {
                            sp.previous();
                        }

                        try {
                            sp.next().enter(context);
                        } catch (ThreadYieldException e) {

                        } catch (StgReturnException e) {
                            /* TODO: Ensure that the StgContext objects
                                     match. */
                        } catch (StackReloadException e) {
                            reload = true;
                            /* TODO: Currently, this reload logic assumes that
                                     the tso doesn't change. Remove this when
                                     confirmed. */
                        } finally {
                            /* TODO: Is this the right way to grab the
                                     context? */
                            ret = context.ret;
                            cap = context.myCapability;
                        }
                        /* TODO: Handle a stack overflow by resetting the
                           stack and pushing the top most closure
                           into an enter frame and restarting.
                           Make sure you check whether this is the
                           first time, otherwise, a infinite loop
                           will occur. */
                    } while (reload);
                    break;
                case ThreadInterpret:
                    cap = interpretBCO(cap);
                    ret = cap.context.ret;
                    break;
                default:
                    barf("schedule: invalid what_next field");
                }

                cap.inHaskell = false;
                t = cap.context.currentTSO;
                cap.context.currentTSO = null;

                if (ret == ThreadBlocked) {
                    if (t.whyBlocked == BlockedOnBlackHole) {
                        //StgTSO owner = blackHoleOwner((MessageBlackHole)t.blockInfo.bh);

                    } else {
                    }
                } else {
                }

                boolean readyToGC = false;

                cap.postRunThread(t);
                switch (ret) {
                case HeapOverflow:
                    readyToGC = cap.handleHeapOverflow(t);
                    break;
                case StackOverflow:
                    cap.threadStackOverflow(t);
                    cap.pushOnRunQueue(t);
                    break;
                case ThreadYielding:
                    if (cap.handleYield(t, prevWhatNext)) {
                        runThread = true;
                    }
                    break;
                case ThreadBlocked:
                    t.handleThreadBlocked();
                    break;
                case ThreadFinished:
                    if (cap.handleThreadFinished(task, t)) return cap;
                    break;
                default:
                    barf("schedule: invalid thread return code %d", ret);
                }

                if (readyToGC && !runThread) {
                    //cap = scheduleDoGc(cap, task, false);
                }
            }
        }
    }

    public final void migrateThread(StgTSO tso, Capability to) {
        tso.whyBlocked = ThreadMigrating;
        tso.cap = to;
        tryWakeupThread(tso);
    }

    public final void appendToRunQueue(StgTSO tso) {
        runQueue.offer(tso);
    }

    public final void pushOnRunQueue(StgTSO tso) {
        runQueue.offerFirst(tso);
    }

    public final StgTSO popRunQueue() {
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

    public final StgTSO peekRunQueue() {
        return runQueue.peek();
    }

    public final void pushWork(Task task) {
        // TODO: Incorporate the spark logic
        if (!RtsFlags.ParFlags.migrate) return;
        if (emptyRunQueue()) {
            if (sparkPoolSize() < 2) return;
        } else {
            if (singletonRunQueue() && sparkPoolSize() < 1) return;
        }
        ArrayList<Capability> freeCapabilities = new ArrayList<Capability>(capabilities.size());
        for (Capability cap: capabilities) {
            if (cap != this && !cap.disabled && cap.tryGrab(task)) {
                if (!cap.emptyRunQueue()
                    || !cap.returningTasks.isEmpty()
                    || !cap.emptyInbox()) {
                    cap.release();
                } else {
                    freeCapabilities.add(cap);
                }
            }
        }
        int nFreeCapabilities = freeCapabilities.size();
        if (nFreeCapabilities > 0) {
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("Capability[%d]: %s and %d free capabilities, sharing..."
                           ,this.no
                           ,(!emptyRunQueue() && !singletonRunQueue())?
                           "excess threads on run queue":"sparks to share (>=2)",
                           nFreeCapabilities);
            }
            int i = 0;
            if (!emptyRunQueue()) {
                // TODO: Object pool this?
                Deque<StgTSO> newRunQueue = new ArrayDeque<StgTSO>(1);
                newRunQueue.offer(popRunQueue());
                StgTSO t = peekRunQueue();
                StgTSO next = null;
                for (; t != null; t = next) {
                    next = popRunQueue();
                    if (t.bound == task.incall
                        || t.isFlagLocked()) {
                        newRunQueue.offer(t);
                    } else if (i == nFreeCapabilities) {
                        i = 0;;
                        newRunQueue.offer(t);
                    } else {
                        Capability freeCap = freeCapabilities.get(i);
                        freeCap.appendToRunQueue(t);
                        if (t.bound != null) {
                            t.bound.task().cap = freeCap;
                        }
                        t.cap = freeCap;
                        i++;
                    }
                }
                runQueue = newRunQueue;
            }

            for (Capability freeCap: freeCapabilities) {
                task.cap = freeCap;
                freeCap.releaseAndWakeup();
            }
        }
        task.cap = this;
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

    public final Capability findWork() {
        Capability cap = this;
        startSignalHandlers();

        if (!RtsFlags.ModeFlags.threaded) {
            checkBlockedThreads();
        } else {
            cap = processInbox();
            if (emptyRunQueue()) activateSpark();
        }

        return cap;
    }

    public final void activateSpark() {}

    public final void startSignalHandlers() {
        if (!RtsFlags.ModeFlags.threaded && RtsFlags.ModeFlags.userSignals) {

            if (RtsFlags.MiscFlags.installSignalHandlers /* && signals_pending() */) {
                // start signal handlers
                //addShutdownHook
            }
        }
    }

    public final void checkBlockedThreads() {
        if (!emptyBlockedQueue() || !emptySleepingQueue()) {
            RtsIO.awaitEvent(emptyRunQueue());
        }
    }

    public final Capability processInbox() {
        Capability cap = this;
        while (!cap.emptyInbox()) {
            // scheduleDoGC
            Lock l = cap.lock;
            if (l.tryLock()) {
                Deque<Message> inbox = new ArrayDeque<Message>(cap.inbox);
                try {
                    cap.inbox.clear();
                } finally {
                    l.unlock();
                }
                for (Message m: inbox) {
                    while (m.isLocked()) {}
                    if (!m.isValid()) continue;
                    m.execute(this);
                }
            } else {
                break;
            }
        }
        return cap;
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

    public final boolean emptyThreadQueues() {
        // TODO: More optimal way of representing this?
        if (RtsFlags.ModeFlags.threaded) {
            return emptyRunQueue();
        } else {
            return emptyRunQueue() && emptyBlockedQueue() && emptySleepingQueue();
        }
    }
    public final void threadPaused(StgTSO tso) {
        maybePerformBlockedException(tso);
        if (tso.whatNext == ThreadKilled) return;
        ListIterator<StackFrame> sp = tso.sp;
        int lastPos = sp.previousIndex();
        // Adjust the stack pointer to the top.
        while (sp.hasNext()) {
            sp.next();
        }
        boolean stopLoop = false;
        while (sp.hasPrevious() && !stopLoop) {
            StackFrame frame = sp.previous();
            MarkFrameResult result = frame.mark(this, tso);
            switch (result) {
                case Marked:
                case Stop:
                    stopLoop = true;
                    break;
                default:
                    break;
            }
        }
        // Adjust the stack pointer to before the pause
        while (sp.previousIndex() < lastPos && sp.hasNext()) {
            sp.next();
        }
        while (sp.previousIndex() > lastPos && sp.hasPrevious()) {
            sp.previous();
        }
    }

    public final boolean maybePerformBlockedException(StgTSO tso) {
        boolean noBlockedExceptions = tso.blockedExceptions.isEmpty();
        if (tso.whatNext == ThreadComplete /*|| tso.whatNext == ThreadFinished */) {
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
                    throwToSingleThreaded(msg.target, msg.exception);
                    StgTSO source = msg.source;
                    msg.done();
                    tryWakeupThread(source);
                    return true;
                } while (true);
            }
            return false;
        }
    }

    public final void awakenBlockedExceptionQueue(StgTSO tso) {
        for (MessageThrowTo msg: tso.blockedExceptions) {
            msg.lock();
            if (msg.isValid()) {
                StgTSO source = msg.source;
                msg.done();
                tryWakeupThread(source);
            } else {
                msg.unlock();
            }
        }
        tso.blockedExceptions.clear();
    }

    public final void tryWakeupThread(StgTSO tso) {
        if (RtsFlags.ModeFlags.threaded) {
            if (tso.cap != this) {
                MessageWakeup msg = new MessageWakeup(tso);
                sendMessage(tso.cap, msg);
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("cap %d: message: try wakeup thread %d on cap %d",
                               tso.id, tso.cap.no);
                }
                return;
            }
        }

        boolean unblock = false;

        switch (tso.whyBlocked) {
            case BlockedOnMVar:
            case BlockedOnMVarRead:
                if (!tso.inMVarOperation) {
                    tso.blockInfo = null;
                    unblock = true;
                }
                break;
            case BlockedOnMsgThrowTo:
                MessageThrowTo msg = (MessageThrowTo) tso.blockInfo;
                msg.lock();
                msg.unlock();
                if (msg.isValid()) {
                    if (RtsFlags.DebugFlags.scheduler) {
                        debugBelch("cap %d: thread %d still blocked on throwto (%d)"
                                   , tso.id, msg.id);
                    }
                } else {
                    assert tso.stack.peek().getClass() == BlockThrowToFrame.class;
                    tso.spPop();
                    unblock = true;
                }
                break;
            case BlockedOnBlackHole:
            case BlockedOnSTM:
            case ThreadMigrating:
                unblock = true;
                break;
            default:
                break;
        }

        if (unblock) {
            tso.whyBlocked = NotBlocked;
            appendToRunQueue(tso);
            // can context switch now
        }
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
            debugBelch("Releasing Capability[%d].", this.no);
        }

        if (RtsFlags.ModeFlags.threaded) {
            Task task = runningTask;
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
                StgTSO nextTSO = peekRunQueue();
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
        return sparks != null && sparks.isEmpty();
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

    public final void throwToSingleThreaded(StgTSO tso, StgClosure exception) {
        throwToSingleThreaded__(tso, exception, false, null);
    }

    public final void throwToSingleThreaded_(StgTSO tso, StgClosure exception,
                                       boolean stopAtAtomically) {
        throwToSingleThreaded__(tso, exception, stopAtAtomically, null);
    }

    public final void throwToSingleThreaded__(StgTSO tso, StgClosure exception,
                                        boolean stopAtAtomically,
                                        UpdateFrame stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }

        removeFromQueues(tso);
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public final void suspendComputation(StgTSO tso, UpdateFrame stopHere) {
        throwToSingleThreaded__(tso, null, false, stopHere);
    }

    public final void removeFromQueues(StgTSO tso) {
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

    public final void raiseAsync(StgTSO tso, StgClosure exception,
                           boolean stopAtAtomically, UpdateFrame stopHere) {

        ListIterator<StackFrame> sp = tso.sp;

        // Ensure sp is pointing to top
        while (sp.hasNext()) {
            sp.next();
        }

        StgThunk updatee = null;
        if (stopHere != null) {
            updatee = stopHere.updatee;
        }

        StackFrame top = sp.previous();
        sp.remove();

        // /* Ensure stack has a closure at the top */
        // if (top.getClosure() == null)  {
        //     sp.next();
        //     sp.add(new StgDummyFrame());
        //     sp.previous();
        // }

        /* Find the index of the update frame to stop at */
        int stopIndex = -1;
        if (stopHere != null) {
            stopIndex = tso.stack.lastIndexOf(stopHere);
        }

        boolean shouldContinue = true;
        // TODO: Handle the case where top.getClosure() == null
        AtomicReference<StgClosure> topClosure =
            new AtomicReference<StgClosure>(top.getClosure());

        while (shouldContinue && (stopHere == null || sp.previousIndex() > stopIndex)) {
            /* TODO: Make a custom peeking iterator */
            StackFrame frame = sp.previous(); //Utils.peekPrevious(sp);
            shouldContinue = frame.doRaiseAsync(this, tso, exception, stopAtAtomically,
                                                updatee, topClosure);
        }

        // /* Maintain the invariant that sp should always
        //    point to stack top. */
        // while (sp.hasNext()) {
        //     sp.next();
        // }

        if (tso.whyBlocked != NotBlocked) {
            tso.whyBlocked = NotBlocked;
            appendToRunQueue(tso);
        }
    }

    public final boolean messageBlackHole(MessageBlackHole msg) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap: %d message: thread %d blocking on blackhole " + msg.bh
                       , no, msg.tso.id, msg.bh);
        }

        StgThunk bh = msg.bh;
        if (bh.getEvaluated() == null) {
            boolean newBlockingQueue;
            do {
                newBlockingQueue = bh.indirectee.blackHole(bh, this, msg);
            } while (newBlockingQueue);
            return true;
        }
        return false;
    }

    public final void checkBlockingQueues(StgTSO tso) {
        if (RtsFlags.DebugFlags.scheduler) {
            debugBelch("cap %d: collision occured; checking blockign queues for thread %d", no, tso.id);
        }
        for (StgBlockingQueue bq: tso.blockingQueues) {
            if (bq.bh.getEvaluated() != null) {
                wakeBlockingQueue(bq);
            }
        }
    }

    public final void updateThunk(StgTSO tso, StgThunk thunk, StgClosure val) {
        StgClosure v = thunk.indirectee;
        thunk.updateWithIndirection(val);
        /* Only if it's an StgIndStatic, do doUpdateThunk */
        if (v != null && v.getEvaluated() == null) {
            v.doUpdateThunk(this, tso);
        }
    }

    public final void wakeBlockingQueue(StgBlockingQueue blockingQueue) {
        for (MessageBlackHole msg: blockingQueue) {
            if (msg.isValid())
                tryWakeupThread(msg.tso);
        }
        blockingQueue.clear();
        // do cleanup here to destroy the BQ;
    }

    public final SchedulerStatus getSchedStatus() {
        return runningTask.incall.returnStatus;
    }

    public final static Capability getFreeRunningCapability() {
        for (Capability cap: capabilities) {
            if (cap.emptyRunQueue() && cap.runningTask != null && !cap.inHaskell) {
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
        //debugTrace
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
        StgTSO tso = context.currentTSO;
        tso.whatNext = ThreadRunGHC;
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
            inHaskell = false;
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
        StgTSO tso = incall.suspendedTso;
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
        cap.inHaskell = true;
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

    public final boolean handleThreadFinished(Task task, StgTSO t) {
        awakenBlockedExceptionQueue(t);

        if (t.bound != null) {
            if (t.bound != task.incall) {
                if (RtsFlags.ModeFlags.threaded) {
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

    public final boolean handleYield(StgTSO t, WhatNext prevWhatNext) {
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

    public final void threadStackOverflow(StgTSO tso) {
        // TODO: Do stack related stuff here
    }

    public final boolean handleHeapOverflow(StgTSO tso) {
        // TODO: Do GC-related stuff here
        return false;
    }

    public final HaskellResult ioManagerStart() {
        return new HaskellResult(this, null);
        /* TODO: Implement IO Manager, Rts.evalIO(this, null /* base_GHCziConcziIO_ensureIOManagerIsRunning_closure */
    }

    public final void postRunThread(StgTSO t) {
        if (t.trec != null && t.whyBlocked == NotBlocked) {
            if (!stmValidateNestOfTransactions(t.trec)) {
                throwToSingleThreaded_(t, null, true);
            }
        }
    }

    public final void promoteInRunQueue(StgTSO tso) {
        removeFromRunQueue(tso);
        pushOnRunQueue(tso);
    }

    public final void removeFromRunQueue(StgTSO tso) {
        runQueue.remove(tso);
    }

    public final MessageThrowTo throwTo(StgTSO source, StgTSO target, StgClosure exception) {
        MessageThrowTo msg = new MessageThrowTo(source, target, exception);
        msg.lock();
        boolean result = throwToMsg(msg);
        if (result) {
            msg.unlock();
            return null;
        } else {
            return msg;
        }
    }

    public final boolean throwToMsg(MessageThrowTo msg) {
        StgTSO target = msg.target;
        // retry: write_barrier
        retry: do {
            if (RtsFlags.DebugFlags.scheduler) {
                debugBelch("throwTo: retrying...");
            }
            if (target.whatNext != ThreadComplete
                && target.whatNext != ThreadKilled) {
                Capability targetCap = target.cap;
                if (target.cap != this) {
                    sendMessage(targetCap, msg);
                } else {
                    WhyBlocked status = target.whyBlocked;
                    switch (status) {
                        case NotBlocked:
                            if (!target.hasFlag(TSO_BLOCKEX)) {
                                raiseAsync(target, msg.exception, false, null);
                            } else {
                                target.blockedExceptions.offer(msg);
                                return false;
                            }
                            break;
                        case BlockedOnMsgThrowTo:
                            MessageThrowTo m = (MessageThrowTo) target.blockInfo;
                            if (m.id < msg.id) {
                                m.lock();
                            } else {
                                if (!m.tryLock()) {
                                    sendMessage(targetCap, msg);
                                    return false;
                                }
                            }
                            if (!msg.isValid()) {
                                m.unlock();
                                tryWakeupThread(target);
                                continue retry;
                            }
                            //TODO: Extra if check?
                            if (target.hasFlag(TSO_BLOCKEX)
                                && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                                m.unlock();
                                target.blockedExceptions.offer(msg);
                                return false;
                            }
                            m.done();
                            raiseAsync(target, msg.exception, false, null);
                            break;
                        case BlockedOnMVar:
                        case BlockedOnMVarRead:
                            StgMVar mvar = (StgMVar) target.blockInfo;
                            // if not mvar retry
                            mvar.lock();
                            if ((target.whyBlocked != BlockedOnMVar &&
                                target.whyBlocked != BlockedOnMVarRead)
                                || target.blockInfo != mvar) {
                                mvar.unlock();
                                continue retry;
                            } else if (!target.inMVarOperation) {
                                mvar.unlock();
                                tryWakeupThread(target);
                                continue retry;
                            } else if (target.hasFlag(TSO_BLOCKEX)
                                    && !target.hasFlag(TSO_INTERRUPTIBLE)) {
                                target.blockedExceptions.offer(msg);
                                mvar.unlock();
                                return false;
                            } else {
                                target.removeFromMVarBlockedQueue();
                                raiseAsync(target, msg.exception, false, null);
                                mvar.unlock();
                            }
                            break;
                        case BlockedOnBlackHole:
                            if (target.hasFlag(TSO_BLOCKEX)) {
                                target.blockedExceptions.offer(msg);
                                return false;
                            } else {
                                ((MessageBlackHole)target.blockInfo).invalidate();
                                raiseAsync(target, msg.exception, false, null);
                            }
                            break;
                        case BlockedOnSTM:
                            target.lock();
                            if (target.whyBlocked != BlockedOnSTM) {
                                target.unlock();
                                continue retry;
                            } else if (target.hasFlag(TSO_BLOCKEX) &&
                                    !target.hasFlag(TSO_INTERRUPTIBLE)) {
                                target.blockedExceptions.offer(msg);
                                target.unlock();
                                return false;
                            } else {
                                raiseAsync(target, msg.exception, false, null);
                                target.unlock();
                            }
                            break;
                        case BlockedOnJavaCall_Interruptible:
                            // check for THREADED_RTS
                            Task task = null;
                            for (Task.InCall incall: suspendedJavaCalls) {
                                if (incall.suspendedTso == target) {
                                    task = incall.task();
                                    break;
                                }
                            }
                            if (task != null) {
                                target.blockedExceptions.offer(msg);
                                if (!(target.hasFlag(TSO_BLOCKEX) &&
                                    !target.hasFlag(TSO_INTERRUPTIBLE))) {
                                    task.interruptWorker();
                                }
                                return false;
                            } else {
                                //debug output
                            }
                        case BlockedOnJavaCall:
                            target.blockedExceptions.offer(msg);
                            return false;
                        case BlockedOnRead:
                        case BlockedOnWrite:
                        case BlockedOnDelay:
                            if (target.hasFlag(TSO_BLOCKEX) &&
                                !target.hasFlag(TSO_INTERRUPTIBLE)) {
                                target.blockedExceptions.offer(msg);
                                return false;
                            } else {
                                removeFromQueues(target);
                                raiseAsync(target, msg.exception, false, null);
                            }
                            break;
                        case ThreadMigrating:
                            tryWakeupThread(target);
                            continue retry;
                        default:
                            barf("throwTo: unrecognized why_blocked (%p)", target.whyBlocked);
                    }
                }
            }
            return true;
        } while (true);
    }

    /* STM Operations */
    public final StgClosure stmReadTvar(StgTRecHeader trec, StgTVar tvar) {
        StgClosure result;
        EntrySearchResult searchResult = STM.getEntry(trec, tvar);
        StgTRecHeader entryIn = searchResult.header;
        TRecEntry entry = searchResult.entry;
        if (entry == null) {
            if (entryIn != trec) {
                TRecEntry newEntry = getNewEntry(trec);
                newEntry.tvar = tvar;
                newEntry.expectedValue = entry.expectedValue;
                newEntry.newValue = entry.newValue;
            }
            result = entry.newValue;
        } else {
            StgClosure currentValue = STM.readCurrentValue(trec, tvar);
            TRecEntry newEntry = getNewEntry(trec);
            newEntry.tvar = tvar;
            newEntry.expectedValue = currentValue;
            newEntry.newValue = currentValue;
            result = currentValue;
        }
        return result;
    }

    public final TRecEntry getNewEntry(StgTRecHeader t) {
        StgTRecChunk c = t.chunkStack.peek();
        TRecEntry entry = new TRecEntry();
        if (c.entries.size() <= TREC_CHUNK_NUM_ENTRIES) {
            c.entries.add(entry);
        } else {
            c = getNewTRecChunk();
            c.entries.add(entry);
            t.chunkStack.push(c);
        }
        return entry;
    }

    public final StgTRecChunk getNewTRecChunk() {
        StgTRecChunk result = null;
        if (freeTRecChunks.isEmpty()) {
            result = new StgTRecChunk();
        } else {
            result = freeTRecChunks.pop();
            result.reset();
        }
        return result;
    }

    public final void stmWriteTvar(StgTRecHeader trec, StgTVar tvar, StgClosure newValue) {
        EntrySearchResult searchResult = STM.getEntry(trec, tvar);
        StgTRecHeader entryIn = searchResult.header;
        TRecEntry entry = searchResult.entry;
        if (entry == null) {
            if (entryIn == trec) {
                entry.newValue = newValue;
            }else {
                TRecEntry newEntry = getNewEntry(trec);
                newEntry.tvar = tvar;
                newEntry.expectedValue = entry.expectedValue;
                newEntry.newValue = newValue;
            }
        } else {
            StgClosure currentValue = STM.readCurrentValue(trec, tvar);
            TRecEntry newEntry = getNewEntry(trec);
            newEntry.tvar = tvar;
            newEntry.expectedValue = currentValue;
            newEntry.newValue = newValue;
        }
    }

    public final void stmAddInvariantToCheck(StgTRecHeader trec, StgClosure code) {
        StgAtomicInvariant invariant = new StgAtomicInvariant(code);
        StgInvariantCheck invariantCheck = getNewInvariantCheck(invariant);
        trec.invariantsToCheck.offer(invariantCheck);
    }

    public final StgInvariantCheck getNewInvariantCheck(StgAtomicInvariant invariant) {
        // TODO: Finish implementation
        StgInvariantCheck result = null;
        if (freeInvariantChecksQueue.isEmpty()) {
            result = new StgInvariantCheck(invariant);
        } else {
            result = freeInvariantChecksQueue.poll();
            result.invariant = invariant;
            result.myExecution = null;
        }
        return result;
    }

    public final boolean stmCommitNestedTransaction(StgTRecHeader trec) {
        STM.lock(trec);
        StgTRecHeader et = trec.enclosingTrec;
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
                        StgTVar s = e.tvar;
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

    public final boolean validateAndAcquireOwnership(StgTRecHeader trec, boolean acquireAll, boolean retainOwnership) {
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
                        StgTVar s = e.tvar;
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

    public final void revertOwnership(StgTRecHeader trec, boolean revertAll) {
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    if (revertAll || e.isUpdate()) {
                        StgTVar s = e.tvar;
                        if (s.isLocked(trec)) {
                            s.unlock(trec, e.expectedValue, true);
                        }
                    }

                }
            }
        }
    }

    public final void freeTRecHeader(StgTRecHeader trec) {
        /* TODO: Verify logic is correct */
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();
        StgTRecChunk currentChunk = cit.previous();
        StgTRecChunk chunk = currentChunk;
        while (cit.hasPrevious()) {
            StgTRecChunk prevChunk = cit.previous();
            freeTRecChunk(chunk);
            chunk = prevChunk;
        }
        /* Clean out all chunks but one */
        cit = trec.chunkStack.listIterator();
        chunk = cit.next();
        while (cit.hasNext()) {
            cit.remove();
            chunk = cit.next();
        }
        currentChunk.reset();
        trec.invariantsToCheck.clear();
        freeTRecHeaders.offer(trec);
    }

    public final void freeTRecChunk(StgTRecChunk chunk) {
        freeTRecChunks.push(chunk);
    }

    public final StgTRecHeader stmStartTransaction(StgTRecHeader outer) {
        getToken();
        return getNewTRecHeader(outer);
    }

    public final void getToken() {
        if (RtsFlags.ModeFlags.threaded) {
            if (transactionTokens == 0) {
                getTokenBatch();
            }
            transactionTokens--;
        }
    }

    public final void getTokenBatch() {
        // TODO: Ensure the cas condition is correct
        while (STM.tokenLocked.compareAndSet(false, true) == true) {}
        STM.maxCommits += STM.TOKEN_BATCH_SIZE;
        transactionTokens = STM.TOKEN_BATCH_SIZE;
        STM.tokenLocked.set(false);
    }

    public final StgTRecHeader getNewTRecHeader(StgTRecHeader enclosingTrec) {
        StgTRecHeader result = null;
        if (freeTRecHeaders.isEmpty()) {
            result = new StgTRecHeader();
        } else {
            result = freeTRecHeaders.poll();
        }
        result.setEnclosing(enclosingTrec);
        return result;
    }

    public final void mergeUpdateInto(StgTRecHeader t, StgTVar tvar, StgClosure expectedValue, StgClosure newValue) {
        boolean found = false;
        ListIterator<StgTRecChunk> cit = t.chunkIterator();
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                if (s == tvar) {
                    found = true;
                    if (e.expectedValue != expectedValue) {
                        t.state = TREC_CONDEMNED;
                    }
                    e.newValue = newValue;
                    break loop;
                }
            }
        }

        if (!found) {
            TRecEntry ne = getNewEntry(t);
            ne.tvar = tvar;
            ne.expectedValue = expectedValue;
            ne.newValue = newValue;
        }
    }

    public final Queue<StgInvariantCheck> stmGetInvariantsToCheck(StgTRecHeader trec) {

        STM.lock(trec);
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                if (e.isUpdate()) {
                    StgTVar s = e.tvar;
                    StgClosure old = s.lock(trec);
                    for (StgClosure q: s.watchQueue) {
                        if (STM.watcherIsInvariant(q)) {
                            boolean found = false;
                            for (StgInvariantCheck q2: trec.invariantsToCheck) {
                                if (q2.invariant == q) {
                                    found = true;
                                    break;
                                }
                            }

                            if (!found) {
                                StgInvariantCheck q3 = getNewInvariantCheck((StgAtomicInvariant) q);
                                trec.invariantsToCheck.offer(q3);
                            }
                        }
                    }
                    s.unlock(trec, old, false);
                }
            }
        }

        STM.unlock(trec);
        return trec.invariantsToCheck;
    }

    public final void stmAbortTransaction(StgTRecHeader trec) {
        STM.lock(trec);
        StgTRecHeader et = trec.enclosingTrec;
        if (et == null) {
            if (trec.state == TREC_WAITING) {
                removeWatchQueueEntriesForTrec(trec);
            }

        } else {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    StgTVar s = e.tvar;
                    mergeReadInto(et, s, e.expectedValue);
                }
            }
        }

        trec.state = TREC_ABORTED;
        STM.unlock(trec);
    }

    public final void removeWatchQueueEntriesForTrec(StgTRecHeader trec) {
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();;
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                StgClosure saw = s.lock(trec);
                s.watchQueue.remove(e.newValue); // TODO: Is this valid?
                s.unlock(trec, saw, false);
            }
        }
    }

    public final void mergeReadInto(StgTRecHeader trec, StgTVar tvar, StgClosure expectedValue) {
        boolean found = false;
        while (!found && trec != null) {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TRecEntry e: chunk.entries) {
                    if (e.tvar == tvar) {
                        found = true;
                        if (e.expectedValue != expectedValue) {
                            trec.state = TREC_CONDEMNED;
                        }
                        break loop;
                    }
                }
            }
            trec = trec.enclosingTrec;
        }

        if (!found) {
            TRecEntry ne = getNewEntry(trec);
            ne.tvar = tvar;
            ne.expectedValue = expectedValue;
            ne.newValue = expectedValue;
        }
    }

    public final boolean stmCommitTransaction(StgTRecHeader trec) {
        STM.lock(trec);
        long maxCommitsAtStart = STM.maxCommits;
        boolean touchedInvariants = !trec.invariantsToCheck.isEmpty();
        if (touchedInvariants) {
            for (StgInvariantCheck q: trec.invariantsToCheck) {
                StgAtomicInvariant inv = q.invariant;
                if (!inv.lock()) {
                    trec.state = TREC_CONDEMNED;
                    break;
                }
                StgTRecHeader invOldTrec = inv.lastExecution;
                if (invOldTrec != null) {
                    ListIterator<StgTRecChunk> cit = invOldTrec.chunkIterator();
                    while (cit.hasPrevious()) {
                        StgTRecChunk chunk = cit.previous();
                        for (TRecEntry e: chunk.entries) {
                            mergeReadInto(trec, e.tvar, e.expectedValue);
                        }
                    }
                }
            }
        }

        boolean useReadPhase = STM.configUseReadPhase && !touchedInvariants;
        boolean result = validateAndAcquireOwnership(trec, !useReadPhase, true);
        if (result) {
            if (useReadPhase) {
                result = trec.checkReadOnly();
                long maxCommitsAtEnd = STM.maxCommits;
                long maxConcurrentCommits = (maxCommitsAtEnd - maxCommitsAtStart) + nCapabilities * STM.TOKEN_BATCH_SIZE;
                if ((maxConcurrentCommits >> 32) > 0 || STM.shake()) {
                    result = false;
                }
            }

            if (result) {
                if (touchedInvariants) {
                    for (StgInvariantCheck q: trec.invariantsToCheck) {
                        StgAtomicInvariant inv = q.invariant;
                        if (inv.lastExecution != null) {
                            inv.disconnect();
                        }
                        q.myExecution.connectInvariant(inv);
                        inv.unlock();
                    }
                }

                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TRecEntry e: chunk.entries) {
                        StgTVar s = e.tvar;
                        if (!useReadPhase || e.newValue != e.expectedValue) {
                            unparkWaitersOn(s);
                            if (RtsFlags.STM.fineGrained) {
                                s.numUpdates++;
                            }
                            s.unlock(trec, e.newValue, true);
                        }
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

    public final void threadStackUnderflow(StgTSO tso) {
        /* TODO: Finish implementation */
    }

    public final StackFrame raiseExceptionHelper(StgTSO tso, StgClosure exception) {
        if (RtsFlags.DebugFlags.printStack) {
            Thread.dumpStack();
        }
        tso.setStackTrace(Thread.currentThread().getStackTrace());
        // Assumes that tso.sp pointer is beyond stack top
        ListIterator<StackFrame> sp = tso.sp;
        boolean shouldContinue = true;
        AtomicReference<StgClosure> raiseClosure = new AtomicReference<StgClosure>();
        StackFrame frame = null;
        while (shouldContinue && sp.hasPrevious()) {
            frame = sp.previous();
            shouldContinue = frame.doRaiseExceptionHelper(this, tso, raiseClosure, exception);
        }
        // TODO: Rethink this when implementing underflow frames/stack chunks
        while (sp.hasNext()) {
            sp.next();
            sp.remove();
        }
        return frame;
    }

    public final void stmCondemnTransaction(StgTRecHeader trec) {
        STM.lock(trec);
        if (trec.state == TREC_WAITING) {
            removeWatchQueueEntriesForTrec(trec);
        }
        trec.state = TREC_CONDEMNED;
        STM.unlock(trec);
    }

    public final void unparkWaitersOn(StgTVar s) {
        Iterator<StgClosure> iterator = s.watchQueue.descendingIterator();
        while (iterator.hasNext()) {
            StgClosure tso = iterator.next();
            if (STM.watcherIsTSO(tso)) {
                unparkTSO((StgTSO) tso);
            }
        }
    }

    public final void unparkTSO(StgTSO tso) {
        tso.lock();
        if (tso.whyBlocked == BlockedOnSTM &&
            tso.blockInfo == STMAwoken.closure) {
            // trace woken up

        } else if (tso.whyBlocked == BlockedOnSTM) {
            tso.blockInfo = STMAwoken.closure;
            tryWakeupThread(tso);
        } else {
            // trace
        }
        tso.unlock();
    }

    public final boolean stmReWait(StgTSO tso) {
        StgTRecHeader trec = tso.trec;
        STM.lock(trec);
        boolean result = validateAndAcquireOwnership(trec, true, true);
        if (result) {
            tso.park();
            revertOwnership(trec, true);
        } else {
            if (trec.state != TREC_CONDEMNED) {
                removeWatchQueueEntriesForTrec(trec);
            }
            freeTRecHeader(trec);
        }
        STM.unlock(trec);
        return result;
    }

    public final StackFrame findRetryFrameHelper(StgTSO tso) {
        ListIterator<StackFrame> sp = tso.sp;
        boolean shouldContinue = true;
        StackFrame frame = null;
        while (shouldContinue) {
            frame = sp.previous();
            /* doFindRetry should expect that the current frame be at sp.next()
               and should return the iterator such that sp.previous() will
               hold the next frame*/
            shouldContinue = frame.doFindRetry(this, tso);
        }
        return frame;
    }

    public final void createSparkThread(Capability cap) {
        StgTSO tso = Rts.createIOThread(cap, null /* TODO: runSparks_closure*/);
        appendToRunQueue(tso);
    }

    public final boolean newSpark(StgClosure p) {
        if (!p.isFizzledSpark()) {
            if (sparks.push(p)) {
                sparkStats.created++;
            } else {
                sparkStats.overflowed++;
            }
        } else {
            sparkStats.dud++;
        }
        return true;
    }

    public final void stmFreeAbortedTrec(StgTRecHeader trec) {
        freeTRecHeader(trec);
    }

    public final boolean stmWait(StgTSO tso, StgTRecHeader trec) {
        STM.lock(trec);
        boolean result = validateAndAcquireOwnership(trec, true, true);
        if (result) {
            buildWatchQueueEntriesForTrec(tso, trec);
            tso.park();
            trec.state = TREC_WAITING;
        } else {
            STM.unlock(trec);
            freeTRecHeader(trec);
        }
        return result;
    }

    public final void buildWatchQueueEntriesForTrec(StgTSO tso, StgTRecHeader trec) {
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TRecEntry e: chunk.entries) {
                StgTVar s = e.tvar;
                /* TODO: Fix order of queue */
                s.watchQueue.offer(tso);
                /* NOTE: The original implementation sets a watchqueue
                         closure */
                e.newValue = tso;
            }
        }
    }

    public final void stmWaitUnlock(StgTRecHeader trec) {
        revertOwnership(trec, true);
        STM.unlock(trec);
    }

    public final boolean stmValidateNestOfTransactions(StgTRecHeader trec) {
        STM.lock(trec);
        StgTRecHeader t = trec;
        boolean result = true;
        while (t != null) {
            result = result && validateAndAcquireOwnership(t, true, false);
            t = t.enclosingTrec;
        }

        if (!result && trec.state != TREC_WAITING) {
            trec.state = TREC_CONDEMNED;
        }
        STM.unlock(trec);
        return result;
    }

    public final StgThunk newCAF(StgIndStatic caf) {
        StgThunk bh = lockCAF(caf);
        if (bh == null) return null;
        if (Thunk.shouldKeepCAFs()) {
            Thunk.revertibleCAFList.offer(caf);
            // Thunk.dynamicCAFList.offer(caf); TODO: Handle when Eta REPL is working
        } else {
            /* TODO: Save the info tables during debugging */
        }
        return bh;
    }

    public final StgThunk lockCAF(StgIndStatic caf) {
        if (RtsFlags.ModeFlags.threaded) {
            StgClosure oldIndirectee = caf.indirectee;
            if (!caf.tryLock(oldIndirectee)) return null;
        }
        StgTSO tso = context.currentTSO;
        StgCAFBlackHole bh = new StgCAFBlackHole(tso);
        caf.indirectee = bh;
        return bh;
    }

    public static void freeCapabilities() {
        if (RtsFlags.ModeFlags.threaded) {
            for (Capability c: capabilities) {
                if (c != mainCapability) {
                    c.free();
                }
            }
        } else {
            mainCapability.free();
        }
    }

    public final void free() {
        if (sparks != null) {
            sparks.discardElements();
        }
    }

    public static void shutdownCapabilities(Task task, boolean safe) {
        for (Capability c: capabilities) {
            c.shutdown(task, safe);
        }
    }

    public final void shutdown(Task task, boolean safe) {
        if (RtsFlags.ModeFlags.threaded) {
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
    }

    public final long sparkPoolSize() {
        if (sparks != null) {
            return sparks.size();
        } else {
            return 0;
        }
    }

    public static StgClosure nonTermination_closure = null;

    static {
        try {
            nonTermination_closure = (StgClosure)
                Class.forName("base.control.exception.Base")
                .getMethod("nonTermination_closure")
                .invoke(null);
        } catch (Exception e) {
            e.printStackTrace();
            nonTermination_closure = null;
        }
    }

    public final void assertFullCapabilityInvariants(Task task) {
        assert runningTask != null && runningTask == task;
        assert task.cap == this;
        assertPartialCapabilityInvariants(task);
    }

    public final void assertPartialCapabilityInvariants(Task task) {
        assert Task.myTask() == task;
        task.assertTaskId();
    }
}
