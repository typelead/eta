package ghcvm.runtime.types;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Deque;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import ghcvm.runtime.types.Task.InCall;
import static ghcvm.runtime.Rts.*;
import static ghcvm.runtime.RtsMessages.*;
import static ghcvm.runtime.Rts.ExitCode.*;
import static ghcvm.runtime.RtsScheduler.*;
import static ghcvm.runtime.RtsScheduler.SchedulerState.*;
import static ghcvm.runtime.RtsScheduler.SchedulerStatus.*;
import static ghcvm.runtime.types.StgTSO.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.*;
import static ghcvm.runtime.closure.StgContext.*;
import static ghcvm.runtime.closure.StgContext.ReturnCode.*;
import static ghcvm.runtime.RtsScheduler.RecentActivity.*;
import static ghcvm.runtime.Interpreter.*;
import ghcvm.runtime.*;
import ghcvm.runtime.thread.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.exception.*;
import ghcvm.runtime.prim.*;
import ghcvm.runtime.message.*;
import ghcvm.runtime.stackframe.*;

public final class Capability {
    public static final int MAX_SPARE_WORKERS = 6;
    public static int nCapabilities;
    public static int enabledCapabilities;
    public static Capability mainCapability;
    public static Capability lastFreeCapability;
    public static List<Capability> capabilities;
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
    public boolean inHaskell;
    public int idle;
    public boolean disabled;
    public Deque<StgTSO> runQueue = new ArrayDeque<StgTSO>();
    public Deque<InCall> suspendedJavaCalls = new ArrayDeque<InCall>();
    public boolean contextSwitch;
    public boolean interrupt;
    public Queue<Task> spareWorkers = new ArrayBlockingQueue<Task>(MAX_SPARE_WORKERS);
    public Deque<Task> returningTasks = new ArrayDeque<Task>();
    public Deque<Message> inbox = new ArrayDeque<Message>();
    public SparkPool sparks = new SparkPool();
    public SparkCounters sparkStats = new SparkCounters();
    public List<StgWeak> weakPtrList = new ArrayList<StgWeak>();
    public int ioManagerControlWrFd; // Not sure if this is necessary

    public Capability(int i) {
        this.no = i;
    }


    public final Capability schedule(Task task) {
        Capability cap = this;
        boolean threaded = RtsFlags.ModeFlags.threaded;
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
                    cap = null;//TODO: scheduleDoGC(cap, task, false);
                case SCHED_SHUTTING_DOWN:
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
                cap = cap.scheduleYield(task);
                if (cap.emptyRunQueue()) continue;
            }

            StgTSO t = cap.popRunQueue();
            if (threaded) {
                Task.InCall bound = t.bound;
                if (bound != null) {
                    if (bound.task() != task) {
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
                context.currentTSO = t;
                context.myCapability = cap;
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
                    context.myCapability = cap;
                    Iterator<StackFrame> it = t.stack.descendingIterator();
                    context.it = it;
                    try {
                        it.next().enter(context);
                    } catch (ThreadYieldException e) {

                    } catch (StgReturnException e) {
                        // Ensure that the StgContext objects match.
                    } finally {
                        // TOOD: is this the right way to grab the context?
                        ret = context.ret;
                        cap = context.myCapability;
                    }
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
        l.lock();
        try {
            if (task.isWorker()) {
                cap.enqueueWorker();
            }
            cap.release_(false);
            if (task.isWorker() || task.isBound()) {
                l.unlock();
                cap = task.waitForWorkerCapability();
            } else {
                cap.newReturningTask(task);
                l.unlock();
                cap = task.waitForReturnCapability();
            }
        } finally {
            l.unlock();
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
            if (sparks.size() < 2) return;
        } else {
            if (singletonRunQueue() && sparks.size() < 1) return;
        }
        ArrayList<Capability> freeCapabilities = new ArrayList(capabilities.size());
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
            int i = 0;
            if (!emptyRunQueue()) {
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
                //scheduleDoGC

            } else {
                if (task.incall.tso != null) {
                    switch (task.incall.tso.whyBlocked) {
                        case BlockedOnSTM:
                        case BlockedOnBlackHole:
                        case BlockedOnMsgThrowTo:
                        case BlockedOnMVar:
                        case BlockedOnMVarRead:
                            cap.throwToSingleThreaded(task.incall.tso,
                                                  null /* TODO: nonTermination_closure */
                                                  );
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
        cap = processInbox();

        if (!RtsFlags.ModeFlags.threaded) {
            checkBlockedThreads();
        } else {
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

    public final Capability processInbox() {return this;}

    public final void startWorkerTask() {
        Task task = new Task(true);
        task.initialize();
        Lock l = task.lock;
        l.lock();
        try {
            task.cap = this;
            runningTask = task;
            WorkerThread thread = new WorkerThread(task);
            thread.start();
            task.id = thread.getId();
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
        // TODO: Implement rest
    }

    public final boolean maybePerformBlockedException(StgTSO tso) {
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

    public final void awakenBlockedExceptionQueue(StgTSO tso) {

    }
    public final void tryWakeupThread(StgTSO tso) {
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

    public final void sendMessage(Capability target, Message msg) {
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

    public final void release_(boolean alwaysWakeup) {
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
        StgTSO nextTSO = peekRunQueue();
        if (nextTSO.bound != null) {
            task = nextTSO.bound.task();
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
            task = spareWorkers.poll();
            if (task != null) {
                giveCapabilityToTask(task);
                return;
            }
        }

        lastFreeCapability = this;
        //debugTrace;
    }

    public final void release() {
        lock.lock();
        try {
            release_(false);
        } finally {
            lock.unlock();
        }
    }

    public final void releaseAndWakeup() {
        lock.lock();
        try {
            release_(true);
        } finally {
            lock.unlock();
        }
    }

    public final void giveCapabilityToTask(Task task) {
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

    public final void throwToSingleThreaded(StgTSO tso, StgClosure exception) {
        throwToSingleThreaded__(tso, exception, false, null);
    }

    public final void throwToSingleThreaded_(StgTSO tso, StgClosure exception,
                                       boolean stopAtAtomically) {
        throwToSingleThreaded__(tso, exception, stopAtAtomically, null);
    }

    public final void throwToSingleThreaded__(StgTSO tso, StgClosure exception,
                                        boolean stopAtAtomically,
                                        StgUpdateFrame stopHere) {
        if (tso.whatNext == ThreadComplete || tso.whatNext == ThreadKilled) {
            return;
        }

        removeFromQueues(tso);
        raiseAsync(tso, exception, stopAtAtomically, stopHere);
    }

    public final void suspendComputation(StgTSO tso, StgUpdateFrame stopHere) {
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
                           boolean stopAtAtomically, StgUpdateFrame stopHere) {
        //debugTrace
        // TODO: Implement later
    }

    public final boolean messageBlackHole(MessageBlackHole msg) {
        // TODO: Implement
        return false;
    }

    public final void checkBlockingQueues(StgTSO tso) {
        // TODO: Impelement
    }

    public final void updateThunk(StgTSO tso, StgInd thunk, StgClosure val) {
        thunk.updateWithIndirection(val);
        if (thunk.isBlackHole()) {
            thunk.indirectee.thunkUpdate(this, tso);
        }
    }

    public final void wakeBlockingQueue(StgBlockingQueue blockingQueue) {
        for (MessageBlackHole msg: blockingQueue) {
            if (msg.isValid()) tryWakeupThread(msg.tso);
        }
        // do cleanup here to destroy the BQ;
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
            return lastFreeCapability;
        } else {
            return lastFreeCapability;
        }
    }

    public final void newReturningTask(Task task) {
        returningTasks.addLast(task);
    }

    public final void popReturningTask() {
        returningTasks.pollFirst();
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
        if ((tso.flags & TSO_BLOCKEX) == 0) {
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
        try {
            if (runningTask != null) {
                lock.unlock();
                return false;
            } else {
                task.cap = this;
                runningTask = task;
            }
        } finally {
            lock.unlock();
        }
        return true;
    }

    public final void enqueueWorker() {
        Task task = runningTask;
        boolean taken = spareWorkers.offer(task);
        if (!taken) {
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
                // should only happen in THREADED_RTS
                appendToRunQueue(t);
                return false;
            }

            if (t.whatNext == ThreadComplete) {
                if (task.incall.ret != null) {
                    StgEnter enterFrame = (StgEnter) task.incall.tso.stack.peek();
                    task.incall.ret = enterFrame.closure;
                }
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
        return Rts.evalIO(this, null /* base_GHCziConcziIO_ensureIOManagerIsRunning_closure */);
    }

    public final void postRunThread(StgTSO t) {
        if (/*t.trec != NO_TREC && */ false && t.whyBlocked == NotBlocked) {
            /* TODO: Implement when implementing STM */
        }
    }
}
