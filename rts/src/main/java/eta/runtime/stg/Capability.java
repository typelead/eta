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
import java.util.concurrent.ThreadLocalRandom;

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
import eta.runtime.storage.Block;
import eta.runtime.storage.LocalHeap;
import eta.runtime.thunk.BlockingQueue;
import eta.runtime.thunk.Thunk;
import eta.runtime.thunk.UpdateInfo;
import eta.runtime.thunk.WhiteHole;
import eta.runtime.util.MPSCLongQueue;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.RuntimeLogging.*;

public final class Capability implements LocalHeap {
    public static final long startTimeNanos = System.nanoTime();
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
            cap = Capability.create(worker);
            myCapability.set(cap);
        }
        return cap;
    }

    private static Capability create(boolean worker) {
        Capability cap = new Capability(Thread.currentThread(), worker);
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
    public final StgContext context   = new StgContext();
    public Deque<TSO> runQueue  = new LinkedList<TSO>();
    public int  lastWorkSize;
    public long lastBlockCheck;
    public int  lastBlockCounter = 0;
    private ThreadLocalRandom tlr = ThreadLocalRandom.current();
    public Deque<Message> inbox = new ConcurrentLinkedDeque<Message>();

    /* MemoryManager related stuff */
    public Block activeDirectBlock;
    public Block activeHeapBlock;
    public Block activeDirectSuperBlock;
    public Block activeHeapSuperBlock;

    public MPSCLongQueue freeMessages = new MPSCLongQueue();
    public long freeSequence;

    public volatile boolean interrupt;

    public Capability(Thread t, boolean worker) {
        this.thread = new WeakReference<Thread>(t);
        this.worker = worker;
    }

    public static Closure scheduleClosure(Closure p) throws java.lang.Exception {
        return getLocal().schedule(new TSO(p));
    }

    /* TODO: Break up this schedule function into chunks for better JIT. */
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
                tryStealGlobalRunQueue();
                if (emptyRunQueue()) {
                    activateSpark();
                    if (emptyRunQueue()) {
                        if (worker && workerCapabilitiesSize() >
                            Runtime.getMaxWorkerCapabilities()) {
                            /* Terminate this Worker Capability if we've exceeded the
                               limit of maxWorkerCapabilities. */
                            return null;
                        }
                        blockedCapabilities.add(this);

                        if (Runtime.debugScheduler()) {
                            debugScheduler("Blocked!");
                        }

                        do {
                            blockedLoop();
                        } while (blockedCapabilities.contains(this));
                        lastBlockCounter = 0;
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
                        t.closure = Closures.evalLazyIO(t.closure);
                    } catch (java.lang.Exception e) {
                        t.whatNext = ThreadKilled;
                        pendingException = (java.lang.Exception) Exception.normalize(e);
                    }
                    break;
                case ThreadInterpret:
                    Interpreter.interpretBCO(this);
                    break;
                default:
                    barf("Invalid whatNext field for TSO[" + t.id + "] - " + prevWhatNext);
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
                    /* Cleanup resources in the Runtime before throwing the exception
                       again. */
                    t.resetStack();
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
        if (Runtime.debugScheduler()) {
            debugScheduler("Sending message " + msg + " to " + target);
        }
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
        final TSO tso = Runtime.createIOThread(Closures.runSparks);
        if (Runtime.debugScheduler()) {
            debugScheduler("Creating a Spark " + tso);
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
        final UpdateInfo ui = tso.updateInfoStack.markBackwardsFrom(this, tso);
        if (ui != null) {
            Exception.suspendComputation(tso, ui.updatee);
        }
    }

    /* Asychronous Exceptions */

    public final boolean maybePerformBlockedException(TSO tso) {
        final Queue<MessageThrowTo> blockedExceptions = tso.blockedExceptions;
        final boolean noBlockedExceptions = blockedExceptions.isEmpty();
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
                final MessageThrowTo msg = tso.blockedExceptions.peek();
                if (msg == null) return false;
                msg.lock();
                tso.blockedExceptions.poll();
                if (!msg.isValid()) {
                    msg.unlock();
                    continue;
                }
                final TSO source = msg.source;
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
        final Thread t = thread.get();
        final TSO tso = context.currentTSO;
        if (t != null && (tso == null || !tso.hasFlag(TSO_INTERRUPT_IMMUNE))) {
            final boolean debug = Runtime.debugScheduler();
            interrupt = true;
            if (tso == null || tso.interruptible()) {
                if (debug) {
                    final String message = (tso == null)? "waiting for work" :
                      tso + " is blocked because " + tso.whyBlocked;
                    debugScheduler(this + " hard interrupted because " + message);
                }
                t.interrupt();
            } else {
                if (debug) {
                    debugScheduler(this + " soft interrupted while running " + tso + " "
                                   + tso.whyBlocked);
                }
            }
            return true;
        } else {
            return false;
        }
    }

    public final boolean interrupted() {
        if (interrupt) {
            interrupt = false;
            return true;
        } else {
            return false;
        }
    }

    public static void interruptAll() {
        if (Runtime.debugScheduler()) {
            debugScheduler("Interrupting all capabilities.");
        }
        for (Capability c: capabilities) {
            c.interrupt();
        }
    }

    /* Thunk Evaluation */

    public final boolean messageBlackHole(Thunk bh, TSO tso, boolean executingMsg) {
        Closure p = bh.indirectee;
        if (p instanceof TSO) {
            TSO owner = (TSO) p;
            if (owner.cap != this) {
                if (tso.blockInfo != bh) {
                    sendMessage(owner.cap, new MessageBlackHole(tso, bh));
                }
            } else if (!executingMsg) {
                Exception.raise(context, Closures.nonTermination);
            } else {
                BlockingQueue bq = new BlockingQueue(owner, bh, tso);
                /* TODO: Optimize blockingQueues */
                owner.blockingQueues.offer(bq);
                bh.setIndirection(bq);
            }
            return true;
        } else if (p instanceof BlockingQueue) {
            BlockingQueue bq = (BlockingQueue) p;
            TSO owner = bq.owner;
            if (owner.cap != this) {
                if (tso.blockInfo != bh) {
                    sendMessage(owner.cap, new MessageBlackHole(tso, bh));
                }
            } else {
                if (tso != owner) {
                    bq.queue(tso);
                }
            }
            return true;
        } else return false;
    }

    public final void checkBlockingQueues(TSO tso) {
        LinkedList<BlockingQueue> bqs = tso.blockingQueues;
        for (BlockingQueue bq: bqs) {
            Thunk p = bq.bh;
            Closure ind = p.indirectee;
            if (ind != bq) {
                wakeBlockingQueue(bq);
                bqs.remove(bq);
            }
        }
    }

    public final void wakeBlockingQueue(BlockingQueue bq) {
        for (TSO tso = bq.queued; tso != null; tso = tso.link) {
            tryWakeupThread(tso);
        }
        bq.queued = null;
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

    public final void idleLoop(final boolean blocked) {
        final TSO tso = context.currentTSO;
        final boolean validTSO = tso != null;

        processInbox();

        /* TODO: Replace this check elsewhere. It's to detect loops in STM. */
        // if (tso.trec != null && tso.whyBlocked == NotBlocked) {
        //     if (!tso.trec.validateNestOfTransactions()) {
        //         throwToSingleThreaded(tso, null, true);
        //     }
        // }

        if (validTSO) threadPaused(tso);

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

            /* Check if there are any deadlocked MVars or STM transactions. */
            if (validTSO) {
                final WhyBlocked whyBlocked = tso.whyBlocked;
                final Object blockInfo = tso.blockInfo;
                detectMVarDeadlock(whyBlocked, blockInfo);
                detectSTMDeadlock(whyBlocked, blockInfo);
            }
        }
    }

    public final void detectMVarDeadlock(WhyBlocked whyBlocked, Object blockInfo) {
        if (whyBlocked == BlockedOnMVar || whyBlocked == BlockedOnMVarRead) {
            if (!globalWorkToDo()) {
                if (workerCapabilitiesSize() == 0) {
                    if (Runtime.debugMVar()) {
                        debugMVar("BlockedIndefinitelyOnMVar: " + blockInfo.hashCode());
                    }
                    Exception.raise(context, Closures.blockedIndefinitelyOnMVar);
                }
            }
        }
    }

    public final void detectSTMDeadlock(WhyBlocked whyBlocked, Object blockInfo) {
        if (whyBlocked == BlockedOnSTM) {
            if (!globalWorkToDo()) {
                if (workerCapabilitiesSize() == 0) {
                    if (Runtime.debugSTM()) {
                        debugSTM("BlockedIndefinitelyOnSTM: " + blockInfo.hashCode());
                    }
                    Exception.raise(context, Closures.blockedIndefinitelyOnSTM);
                }
            }
        }
    }

    /* Blocked Loop */
    public final void blockedLoop() {
      lastBlockCounter = lastBlockCounter % 10 + 1; /* should be plenty */
      blockedLoop(Runtime.getMaxTSOBlockTimeNanos() * tlr.nextInt(0, 1 << lastBlockCounter));
    }

    public final void blockedLoop(long nanos) {
        idleLoop(true);
        LockSupport.parkNanos(nanos);
        interrupted();
        idleLoop(false);
    }

    public static boolean globalWorkToDo() {
        return !Concurrent.emptyGlobalRunQueue() || Parallel.anySparks();
    }

    public static int globalWorkSize() {
        return Concurrent.getGlobalRunQueueSize() + ((Parallel.anySparks())? 1 : 0);
    }

    public final void manageOrSpawnWorkers() {

        /* When we have excess live threads and blocked Capabilities, let's wake
           them up so they can terminate themselves. */
        if ((workerCapabilitiesSize() > Runtime.getMaxWorkerCapabilities()) &&
            !blockedCapabilities.isEmpty()) {
            unblockCapabilities(0);
        }
            /* Interrupt the blocked capabilities so that they can terminate
               themselves when they unblock. */

        final int currentWorkSize = globalWorkSize();
        if (currentWorkSize > 0) {
            if (!blockedCapabilities.isEmpty()) {
                unblockCapabilities(currentWorkSize);
            } else if (workerCapabilitiesSize() < Runtime.getMaxWorkerCapabilities()) {
                new WorkerThread().start();
            } else if ((System.nanoTime() - lastBlockCheck) >
                       Runtime.getMinTSOIdleTimeNanos()) {
                /* If no work was done since the last block check, spin up a thread,
                   even though it exceeds the limit. */
                if (lastWorkSize <= currentWorkSize) {
                    new WorkerThread().start();
                }
                lastWorkSize   = currentWorkSize;
                lastBlockCheck = System.nanoTime();
            }
        }
    }

    /* Blocked Capabilities
       This stores the Worker Capabilities that are idle.
    */

    public static Set<Capability> blockedCapabilities
        = Collections.newSetFromMap(new ConcurrentHashMap<Capability, Boolean>());
    public static AtomicBoolean blockedCapabilitiesLock = new AtomicBoolean();

    public static void unblockCapabilities(int n) {
        /* TODO: Take into account the amount of work available instead of unblocking
                 everything. */
        /* TODO: Make it easier to pair work with capabilities in data structure form
                 such that contention is reduced. */
        if (!blockedCapabilities.isEmpty()) {
            if (!blockedCapabilitiesLock.get() &&
                blockedCapabilitiesLock.compareAndSet(false, true)) {
                try {
                    for (Capability c:blockedCapabilities) {
                        if (Runtime.debugScheduler()) {
                            debugScheduler("Interrupting blocked capability: " + c);
                        }
                        c.interrupt();
                    }
                    blockedCapabilities.clear();
                } finally {
                    blockedCapabilitiesLock.set(false);
                }
            }
        }
    }

    public final void setActiveBlock(Block block, boolean direct, boolean supr) {
        if (direct) {
            if (supr) {
                block.setLink(activeDirectSuperBlock);
                activeDirectSuperBlock = block;
            } else {
                block.setLink(activeDirectBlock);
                activeDirectBlock = block;
            }
        } else {
            if (supr) {
                block.setLink(activeHeapSuperBlock);
                activeHeapSuperBlock = block;
            } else {
                block.setLink(activeHeapBlock);
                activeHeapBlock = block;
            }
        }
    }

    /* The Capability will attempt to allocate the data with the local resources
       it has. */
    public final long allocateLocal(int miniblocks, boolean direct, boolean _supr) {
        /* We eagerly process free messages to minimize fragmentation. */
        processFreeMessages();
        Block block = direct? activeDirectBlock : activeHeapBlock;
        long address = 0;
        if (block != null) {
            address = Block.findFreeInBlockStack(block, miniblocks, direct);
        }
        if (address == 0) {
            block = direct? activeDirectSuperBlock : activeHeapSuperBlock;
            if (block != null) {
                address = Block.findFreeInBlockStack(block, miniblocks, direct);
            }
        }
        return address;
    }

    private final void processFreeMessages() {
        while (freeMessages.canRead(freeSequence)) {
            MemoryManager.getHeap().free(freeMessages.read());
            freeSequence++;
        }
    }

    public final void cleanupLocalHeap() {
        activeDirectBlock      = null;
        activeHeapBlock        = null;
        activeDirectSuperBlock = null;
        activeHeapSuperBlock   = null;
        freeMessages           = new MPSCLongQueue();
        freeSequence           = 0;
    }

    @Override
    public String toString() {
        String workerString = worker? "[Worker]" : "";
        return "Capability" + workerString + "[" + id + "]";
    }
}
