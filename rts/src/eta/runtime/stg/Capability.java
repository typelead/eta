package eta.runtime.stg;

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Deque;
import java.util.Queue;
import java.util.Set;
import java.util.Collections;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.locks.LockSupport;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import java.lang.ref.WeakReference;

import eta.runtime.Runtime;
import eta.runtime.io.IO;
import eta.runtime.io.MemoryManager;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.concurrent.WorkerThread;
import eta.runtime.exception.Exception;
import eta.runtime.exception.FiberYieldException;
import eta.runtime.interpreter.Interpreter;
import eta.runtime.message.Message;
import eta.runtime.message.MessageBlackHole;
import eta.runtime.message.MessageThrowTo;
import eta.runtime.message.MessageShutdown;
import eta.runtime.message.MessageWakeup;
import eta.runtime.parallel.Parallel;
import eta.runtime.thunk.BlockingQueue;
import eta.runtime.thunk.Thunk;
import eta.runtime.thunk.UpdateInfo;
import eta.runtime.thunk.WhiteHole;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugScheduler;

public final class Capability {
    public static List<Capability> capabilities = new ArrayList<Capability>();
    public static Set<Capability> workerCapabilities
        = Collections.newSetFromMap(new ConcurrentHashMap<Capability, Boolean>());
    public static AtomicInteger workerCapNextId = new AtomicInteger();
    private static ThreadLocal<Capability> myCapability = new ThreadLocal<Capability>();

    public static boolean singletonCapabilities() {
        return capabilities.size() == 1 && workerCapabilities.isEmpty();
    }

    public static int workerCapabilitiesSize() {
        return workerCapabilities.size();
    }

    public static Capability getLocal(boolean worker) {
        Capability cap = myCapability.get();
        if (cap == null) {
            cap = new Capability(Thread.currentThread(), worker);
            if (worker) {
                workerCapabilities.add(cap);
                cap.id = workerCapNextId.getAndIncrement();
            } else {
                /* TODO: Use a concurrent data structure for capabilities */
                synchronized (capabilities) {
                    capabilities.add(cap);
                    cap.id = capabilities.size() - 1;
                }
            }
            myCapability.set(cap);
        }
        return cap;
    }

    public static Capability getLocal() {
        return getLocal(false);
    }

    public static int getNumCapabilities() {
        return Runtime.getMaxWorkerCapabilities();
    }

    public static void setNumCapabilities(int n) {
        Runtime.setMaxWorkerCapabilities(n);
    }

    public int id;
    public final boolean worker;
    public final WeakReference<Thread> thread;
    public StgContext context   = new StgContext();
    public Deque<TSO> runQueue  = new LinkedList<TSO>();
    public Deque<Message> inbox = new ConcurrentLinkedDeque<Message>();

    public Capability(Thread t, boolean worker) {
        this.thread = new WeakReference<Thread>(t);
        this.worker = worker;
    }

    public static Closure scheduleClosure(Closure p) throws java.lang.Exception {
        return getLocal().schedule(new TSO(p));
    }

    public final Closure schedule(TSO tso) throws java.lang.Exception {
        if (tso != null) {
            appendToRunQueue(tso);
        }
        Closure result = null;
        TSO     outer  = null;
        java.lang.Exception pendingException = null;

        do {
            result = null;
            pendingException = null;
            if (context.currentTSO != null) {
                /* Re-entering the RTS, a fresh TSO was generated. */
                outer = context.currentTSO;
            }

            processInbox();

            /* TODO: The following still need to be implemented:
               - Deadlock detection. Be able to detect <<loop>>.
            */
            if (emptyRunQueue()) {

                if (worker && workerCapabilitiesSize() > Runtime.getMaxWorkerCapabilities()) {
                    /* Terminate this Worker Capability if we've exceeded the limit
                       of maxWorkerCapabilities. */
                    return null;
                }

                tryStealGlobalRunQueue();
                if (emptyRunQueue()) {
                    activateSpark();
                    if (emptyRunQueue()) {
                        blockedCapabilities.add(this);
                        do {
                            blockedLoop(Runtime.getMinWorkerCapabilityIdleTimeNanos());
                        } while (blockedCapabilities.contains(this));
                        continue;
                    }
                }
            }

            TSO t = popRunQueue();
            context.reset(this, t);

            WhatNext prevWhatNext = t.whatNext;
            switch (prevWhatNext) {
                case ThreadKilled:
                case ThreadComplete:
                    break;
                case ThreadRun:
                    try {
                        result = t.closure.enter(context);
                    } catch (FiberYieldException fye) {
                        result = null;
                    } catch (java.lang.Exception e) {
                        t.whatNext = ThreadKilled;
                        pendingException = e;
                    }
                    break;
                case ThreadInterpret:
                    Interpreter.interpretBCO(this);
                    break;
                default:
                    barf("Invalid whatNext field for TSO[%d].", t.id);
            }

            context.currentTSO = null;

            if (outer != null) {
                context.currentTSO = outer;
                outer              = null;
            }

            prevWhatNext = t.whatNext;

            if (prevWhatNext == ThreadYield || prevWhatNext == ThreadBlock) {
                if (prevWhatNext == ThreadYield) {
                    Concurrent.pushToGlobalRunQueue(t);
                }
                t.whatNext = ThreadRun;
                t.cap = null;
            } else {
                /* Thread is done executing, awaken the blocked exception queue. */
                awakenBlockedExceptionQueue(t);

                /* If an unhandled exception occured, throw it so that the caller
                   can handle it if they so choose. */
                if (pendingException != null) {
                    /* A TSO has the ability to control the stack trace of an exception. */
                    StackTraceElement[] st = t.getStackTrace();
                    if (st != null) {
                        pendingException.setStackTrace(st);
                        t.resetStack();
                    }
                    /* Cleanup resources in the Runtime before throwing the exception
                       again. */
                    /* TODO: Will we have a difference between cleanup and exit? */
                    if (!worker) {
                        Runtime.exit();
                    }
                    throw pendingException;
                }
            }

            if (emptyRunQueue() && !worker) break;
        } while (true);
        return result;
    }

    /* Run Queue */

    public final boolean emptyRunQueue() {
        return runQueue.isEmpty();
    }

    public final int runQueueSize() {
        return runQueue.size();
    }

    public final void appendToRunQueue(TSO tso) {
        if (!runQueue.contains(tso)) {
            runQueue.offerLast(tso);
        }
        tso.cap = this;
    }

    public final TSO popRunQueue() {
        return runQueue.pollFirst();
    }

    public final TSO peekRunQueue() {
        return runQueue.peekFirst();
    }

    /* Message Inbox */

    public void processInbox() {
        Message msg;
        while ((msg = inbox.poll()) != null) {
            msg.execute(this);
        }
    }

    public final boolean sendMessage(Capability target, Message msg) {
        target.inbox.offer(msg);
        return target.interrupt();
    }

    public final boolean emptyInbox() {
        return inbox.isEmpty();
    }

    /* Sparks */
    public final void activateSpark() {
        if (Parallel.anySparks()) {
            createSparkThread();
        }
    }

    public final void createSparkThread() {
        TSO tso = Runtime.createIOThread(Closures.runSparks);
        if (Runtime.debugScheduler()) {
            debugScheduler("Creating a Spark TSO[%d].", tso.id);
        }
        appendToRunQueue(tso);
    }

    public final boolean newSpark(Closure p) {
        if (p.getEvaluated() == null) {
            if (Parallel.submitSpark(p)) {
                Parallel.globalSparkStats.created.getAndIncrement();
            } else {
                Parallel.globalSparkStats.overflowed.getAndIncrement();
            }
        } else {
            Parallel.globalSparkStats.dud.getAndIncrement();
        }
        idleLoop(false);
        return true;
    }

    /* Lazy Blackholing */

    public final void threadPaused(TSO tso) {
        maybePerformBlockedException(tso);
        UpdateInfo ui = tso.updateInfoStack.markBackwardsFrom(this, tso);
        if (ui != null) {
            Exception.suspendComputation(tso, ui);
        }
    }

    /* Asychronous Exceptions */

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
                Exception.throwToSingleThreaded(msg.target, msg.exception);
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

    /* Communication between Capabilities */

    public final void tryWakeupThread(TSO tso) {
        if (tso.cap != this) {
            sendMessage(tso.cap, new MessageWakeup(tso));
        } else {
            boolean blocked = true;
            switch (tso.whyBlocked) {
                case BlockedOnMVar:
                case BlockedOnMVarRead:
                    /* TODO: fix this */
                    break;
                case BlockedOnMsgThrowTo:
                    MessageThrowTo msg = (MessageThrowTo) tso.blockInfo;
                    if (msg.isValid()) {
                        return;
                    }
                case BlockedOnBlackHole:
                case BlockedOnSTM:
                    break;
                default:
                    blocked = false;
                    break;
            }
            tso.whyBlocked = NotBlocked;
            if (!blocked) {
                appendToRunQueue(tso);
            }
        }
    }

    public final boolean interrupt() {
        Thread t = thread.get();
        TSO tso = context.currentTSO;
        if (t != null && (tso == null || !tso.hasFlag(TSO_INTERRUPT_IMMUNE))) {
            t.interrupt();
            return true;
        } else {
            return false;
        }
    }

    public static void interruptAll() {
        for (Capability c: capabilities) {
            c.interrupt();
        }
    }

    /* Thunk Evaluation */

    public final boolean messageBlackHole(MessageBlackHole msg, boolean executingMsg) {
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
                } else if (!executingMsg) {
                    Exception.raise(context, Closures.nonTermination);
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
                bq.messages.offer(msg);
                return true;
            } else return false;
        } while (true);
    }

    public final void checkBlockingQueues(TSO tso) {
        for (BlockingQueue bq: tso.blockingQueues) {
            Thunk p = bq.bh;
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

    /* Capabilities Cleanup */

    public static void shutdownCapabilities(boolean safe) {
        while (workerCapabilities.size() > 0) {
            for (Capability c: workerCapabilities) {
                c.shutdown(safe);
            }
            LockSupport.parkNanos(1000000L);
        }
    }

    public final boolean shutdown(boolean safe) {
        return sendMessage(this, MessageShutdown.getInstance());
    }

    /* Globan Run Queue Stealing */

    public final TSO tryStealGlobalRunQueue() {
        TSO tso = Concurrent.stealFromGlobalRunQueue();
        if (tso != null) {
            Concurrent.globalRunQueueModifiedTime = System.currentTimeMillis();
            tso.cap = this;
            tryWakeupThread(tso);
        }
        return tso;
    }

    /* Idle Loop */

    public final void idleLoop(boolean blocked) {
        TSO tso = context.currentTSO;
        processInbox();

        /* TODO: Replace this check elsewhere. It's to detect loops in STM. */
        // if (tso.trec != null && tso.whyBlocked == NotBlocked) {
        //     if (!tso.trec.validateNestOfTransactions()) {
        //         throwToSingleThreaded(tso, null, true);
        //     }
        // }

        if (tso != null) threadPaused(tso);

        /* Spawn worker capabilities if there's work to do */
        manageOrSpawnWorkers();

        if (blocked) {
            /* All computations that are intensive and not absolutely required to
               be running at regular intervals should be done here. */

            /* Check for any completed futures and wake up the threads. */
            Concurrent.checkForCompletedFutures(this);

            /* Check for any ready I/O operations and wake up the threads. */
            Concurrent.checkForReadyIO(this);

            /* Free any memory if necessary */
            MemoryManager.maybeFreeNativeMemory();

            /* Check if there are any deadlocked MVars. */
            if (tso != null) detectMVarDeadlock(tso.whyBlocked);
        }
    }

    public final void detectMVarDeadlock(WhyBlocked whyBlocked) {
        if (whyBlocked == BlockedOnMVar || whyBlocked == BlockedOnMVarRead) {
            if (workerCapabilitiesSize() == 0 && !globalWorkToDo()) {
                Exception.raise(context, Closures.blockedIndefinitelyOnMVar);
            }
        }
    }

    /* Blocked Loop */
    public final void blockedLoop() {
        blockedLoop(Runtime.getMaxTSOBlockTimeNanos());
    }

    public final void blockedLoop(long nanos) {
        idleLoop(true);
        LockSupport.parkNanos(nanos);
        Thread.interrupted();
        idleLoop(false);
    }

    public static boolean globalWorkToDo() {
        return (!Concurrent.emptyGlobalRunQueue() || Parallel.anySparks());
    }

    public final void manageOrSpawnWorkers() {

        /* When we have excess live threads and blocked Capabilities, let's wake
           them up so they can terminate themselves. */
        if ((workerCapabilitiesSize() > Runtime.getMaxWorkerCapabilities()) &&
            !blockedCapabilities.isEmpty()) {
            unblockCapabilities();
        }
            /* Interrupt the blocked capabilities so that they can terminate
               themselves when they unblock. */

        if (globalWorkToDo()
            // TODO: Is this timeout really necessary?
            // &&
            // ( System.currentTimeMillis()
            // - Concurrent.globalRunQueueModifiedTime
            // > Runtime.getMinTSOIdleTime())
            ) {
            if (!blockedCapabilities.isEmpty()) {
                unblockCapabilities();
            } else if (workerCapabilitiesSize() < Runtime.getMaxWorkerCapabilities()) {
                new WorkerThread().start();
            }
        }
    }

    /* Blocked Capabilities
       This stores the Worker Capabilities that are idle.
    */

    public static Set<Capability> blockedCapabilities
        = Collections.newSetFromMap(new ConcurrentHashMap<Capability, Boolean>());
    public static AtomicBoolean blockedCapabilitiesLock = new AtomicBoolean();

    public static void unblockCapabilities() {
        /* TODO: Optimization? Only unlock SOME Capabilities to reduce contention on
                 grabbing from the Global Run Queue and Global Spark Pool. */
        /* NOTE: We just move on if we're unable to lock, as we know for sure
                 another thread must be unblocking them anyways. */
        if (!blockedCapabilities.isEmpty()) {
            if (blockedCapabilitiesLock.compareAndSet(false, true)) {
                try {
                    for (Capability c:blockedCapabilities) {
                        c.interrupt();
                    }
                    blockedCapabilities.clear();
                } finally {
                    blockedCapabilitiesLock.set(false);
                }
            }
        }
    }

}
