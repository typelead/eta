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
import static eta.runtime.RuntimeLogging.*;
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
    public static List<Capability> capabilities = new ArrayList<Capability>();
    public static Set<Capability> blockedCapabilities
        = Collections.newSetFromMap(new ConcurrentHashMap<Capability, Boolean>());
    private static ThreadLocal<Capability> myCapability = new ThreadLocal<Capability>();

    public static Capability getLocal() {
        Capability cap = myCapability.get();
        if (cap == null) {
            cap = new Capability(Thread.currentThread());
            synchronized (capabilities) {
                capabilities.add(cap);
                cap.id = capabilities.size() - 1;
            }
        }
        return cap;
    }

    public static void setNumCapabilities(int n) {
        /* TODO: setMaxWorkerCapabilities here */
    }

    public int id;
    public WeakReference<Thread> thread;
    public Lock lock = new ReentrantLock();
    public StgContext context = new StgContext();
    public Deque<TSO> runQueue = new LinkedLinked<TSO>();
    public Deque<Message> inbox = new ConcurrentLinkedDeque<Message>();
    public Map<WeakReference<Closure>, WeakPtr> weakPtrMap = new ArrayList<Weak>();
    public ReferenceQueue<Closure> refQueue = new ReferenceQueue<Closure>();

    public Capability(Thread t) {
        this.thread = new WeakReference<Thread>(t);
    }

    public static Closure scheduleClosure(Closure p) {
        return getLocal().schedule(new TSO(p));
    }

    public static void interruptAll() {
        for (Capability c: capabilities) {
            c.interrupt();
        }
    }

    public final Closure schedule(TSO tso, boolean worker) {
        if (tso != null) {
            cap.appendToRunQueue(tso);
        }
        Closure    result       = null;
        TSO        outer        = null;
        boolean    reEntry      = false;

        do {
            if (context.currentTSO != null) {
                /* Re-entering the RTS, a fresh TSO was generated. */
                outer = context.currentTSO;
                reEntry = true;
            }

            /* TODO: The following still need to be implemented:
               - Deadlock detection. Be able to detect <<loop>>.
            */
            if (emptyRunQueue()) {
                tryStealGlobalRunQueue();
                if (emptyRunQueue()) {
                    activateSpark();
                    if (emptyRunQueue()) {
                        blockedCapabilities.add(this);
                        LockSupport.park();
                        if (Thread.interrupted()) {}
                        if (blockedCapabilities.contains(this)) {
                            blockedCapabilities.remove(this);
                        }
                        continue;
                    }
                }

            }

            TSO t = popRunQueue();
            context.reset(cap, t);

            WhatNext prevWhatNext = t.whatNext;
            switch (prevWhatNext) {
                case ThreadKilled:
                case ThreadComplete:
                    break;
                case ThreadRun:
                    try {
                        result = tso.closure.enter(context);
                    } catch (Exception e) {
                        // TODO: Catch exceptions here?
                        throw e;
                    }
                    break;
                case ThreadInterpret:
                    interpretBCO(cap);
                    break;
                default:
                    barf("{Scheduler} Invalid whatNext field for TSO[%d].", t.id);
            }

            context.currentTSO = null;

            if (reEntry) {
                context.currentTSO = outer;
                reEntry = false;
            }

            /* Thread is done executing, awaken the blocked exception queue. */
            awakenBlockedExceptionQueue(t);
            break;
        } while (true);
        return result;
    }
    public final void findWork() {
    }

    public final void migrateThread(TSO tso, Capability to) {
        tso.whyBlocked = ThreadMigrating;
        tso.cap = to;
        tryWakeupThread(tso);
    }

    /* Run Queue */

    public final boolean emptyRunQueue() {
        return runQueue.isEmpty();
    }

    public final int runQueueSize() {
        return runQueue.size();
    }

    public final void appendToRunQueue(TSO tso) {
        runQueue.offerLast(tso);
    }

    public final void pushOnRunQueue(TSO tso) {
        runQueue.offerFirst(tso);
    }

    public final TSO popRunQueue() {
        return runQueue.pollFirst();
    }

    public final TSO peekRunQueue() {
        return runQueue.peekFirst();
    }

    public final void promoteInRunQueue(TSO tso) {
        removeFromRunQueue(tso);
        pushOnRunQueue(tso);
    }

    public final void removeFromRunQueue(TSO tso) {
        runQueue.remove(tso);
    }

    /* Sparks */

    public final void activateSpark() {
        if (anySparks()) {
            createSparkThread();
            if (RuntimeOptions.DebugFlag.scheduler) {
                debugBelch("{Scheduler} Creating a Spark TSO[%d].", tso.id);
            }
        }
    }

    public static boolean anySparks() {
        return !emptyGlobalRunQueue();
    }

    public final void createSparkThread() {
        appendToRunQueue(Runtime.createIOThread(Closures.runSparks));
    }

    public final boolean newSpark(Closure p) {
        /* TODO: Synchronize the stats */
        if (p.getEvaluated() == null) {
            if (sparks.offerFirst(p)) {
                globalSparkStats.created++;
            } else {
                globalSparkStats.overflowed++;
            }
        } else {
            globalSparkStats.dud++;
        }
        return true;
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
        }

        if (!noBlockedExceptions &&
            (!tso.hasFlag(TSO_BLOCKEX) ||
             (tso.hasFlag(TSO_INTERRUPTIBLE) && tso.interruptible()))) {
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
                return true;
            } while (true);
        }
        return false;
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
        target.inbox.offer(msg);
        target.interrupt();
    }

    public final boolean emptyInbox() {
        return inbox.isEmpty();
    }

    public final void interrupt() {
        Thread t = thread.get();
        if (t != null) t.interrupt();
    }

    public final boolean messageBlackHole(MessageBlackHole msg) {
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
                return true;
            } else return false;
        } while (true);
    }

    public final void checkBlockingQueues(TSO tso) {
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
        if (RuntimeOptions.DebugFlags.scheduler) {
            debugBelch("{Scheduler} Passing Capability[%d] to %s Task[%d].",
                       cap.id, (task.incall.tso != null)? "Bound" : "Worker", task.id);
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

    public final void scheduleWorker() {
        Capability cap = schedule(null);
        /* TODO: Finish this */
    }


    public static void shutdownCapabilities(Task task, boolean safe) {
        for (Capability c: capabilities) {
            c.shutdown(task, safe);
        }
    }

    public TSO tryStealGlobalRunQueue() {
        TSO tso = globalRunQueue.pollLast();
        if (tso != null) {
            migrateThread(tso, this);
        }
    }

    public static void runFinalizers() {
        for (Capability c: Capability.capabilities) {
            c.runAllJavaFinalizers();
        }
    }

    public static void runAllJavaFinalizers() {
        /* TODO: Run finalizers */
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

        /* TODO: Replace this check elsewhere. It's to detect loops in STM. */
        if (tso.trec != null && tso.whyBlocked == NotBlocked) {
            if (!tso.trec.validateNestOfTransactions()) {
                throwToSingleThreaded(tso, null, true);
            }
        }

        if (actuallyBlocked) {
            /* When it's blocked, we can assumed that threadPaused() has
               already been run, hence, we only need to check blocked exceptions. */
            maybePerformedBlockedExceptions(tso);
        } else {
            threadPaused(tso);
        }

        /* Run finalizers for WeakPtrs and ByteArrays */
        checkFinalizers();

        /* Spawn worker capabilities if there's work to do */
        maybeSpawnWorkers();
    }

    public void processInbox() {
        Message msg;
        while ((msg = inbox.poll()) != null) {
            msg.execute(cap);
        }
    }

    public final void scheduleThreadOn(int cpu, TSO tso) {
        tso.addFlags(TSO_LOCKED);
        cpu %= capabilities.size();
        if (cpu == cap.id) {
            appendToRunQueue(tso);
        } else {
            migrateThread(tso, capabilities.get(cpu));
        }
    }
}
