package eta.runtime.stg;

import java.util.LinkedList;
import java.util.Deque;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.locks.LockSupport;
import java.util.concurrent.atomic.AtomicInteger;
import java.io.IOException;
import java.nio.channels.SelectableChannel;
import java.lang.ref.WeakReference;
import eta.runtime.util.MPSCReferenceQueue;

import eta.runtime.Runtime;
import eta.runtime.io.IOManager;
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
import eta.runtime.util.MPSCLongQueue;
import eta.runtime.util.Consumer;
import static eta.runtime.stg.TSO.*;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.RuntimeLogging.*;

public final class Capability implements LocalHeap {

    public static final long startTimeNanos = System.nanoTime();

    /* These are the bound capabilities, i.e. capabilities linked to threads NOT created and
       managed by the Eta RTS. */
    private static MPSCReferenceQueue<Capability> capabilities =
        new MPSCReferenceQueue<Capability>();

    /* The current number of bound capabilities */
    private static AtomicInteger capabilitiesSize = new AtomicInteger();

    public static int capabilitiesSize() {
        return capabilitiesSize.get();
    }

    /* These are the worker capabilities, i.e. capabilities linked to threads created and
       managed by the Eta RTS. */
    private static MPSCReferenceQueue<Capability> workerCapabilities =
        new MPSCReferenceQueue<Capability>();

    /* The current number of worker capabilities */
    private static AtomicInteger workerCapabilitiesSize = new AtomicInteger();

    public static int workerCapabilitiesSize() {
        return workerCapabilitiesSize.get();
    }

    /* Add worker to the queue and assign a unique id. */
    public void addWorker() {
        final long workerSequence = workerCapabilities.write(this);
        this.workerSequence = workerSequence;
        this.id = workerCapabilities.readIndex(workerSequence);
        workerCapabilitiesSize.getAndIncrement();
    }

    /* Remove worker from the queue and re-cycle its id. */
    public void removeWorker() {
        workerCapabilitiesSize.getAndDecrement();
        workerCapabilities.read(workerSequence);
    }

    /* Returns true if the runtime is being used in the concurrent mode. */
    public static boolean singletonCapabilities() {
        return /* WARNING: This check should be changed if the
                           capabilities data structure changes! */
               capabilitiesSize()       == 1
            && workerCapabilitiesSize() == 0;
    }

    /* This object is used to sychronize among all the idle worker capabilities. */
    private static Object blockedLock = new Object();

    /* The current number of worker capabilities waiting for work */
    private static AtomicInteger idleCapabilitiesSize = new AtomicInteger();

    public static int idleCapabilitiesSize() {
        return idleCapabilitiesSize.get();
    }

    /* The thread-local storage for the capability linked to a thread. Is initialized once
       and not modified further. */
    private static ThreadLocal<Capability> myCapability = new ThreadLocal<Capability>();

    /* Get the capability linked to the current thread, initializing it if needed. */

    public static Capability getLocal() {
        return getLocal(false);
    }

    public static Capability getLocal(final boolean worker) {
        Capability cap = myCapability.get();
        if (cap == null) {
            cap = Capability.create(worker);
            myCapability.set(cap);
        }
        return cap;
    }

    public static int getNumCapabilities() {
        return Runtime.getMaxWorkerCapabilities();
    }

    public static void setNumCapabilities(int n) {
        Runtime.setMaxWorkerCapabilities(n);
    }

    private int id;
    private final boolean worker;
    private final WeakReference<Thread> thread;
    private final StgContext context = new StgContext();
    private Deque<TSO> runQueue = new LinkedList<TSO>();
    private Deque<Message> inbox = new ConcurrentLinkedDeque<Message>();
    private IOManager ioManager;

    /* MemoryManager related stuff */
    private Block activeDirectBlock;
    private Block activeHeapBlock;
    private Block activeDirectSuperBlock;
    private Block activeHeapSuperBlock;

    private MPSCLongQueue freeMessages = new MPSCLongQueue();
    private long freeSequence;

    private long workerSequence;

    private volatile boolean interrupt;
    private volatile Capability link;

    public Capability(final Thread t, final boolean worker) {
        this.thread    = new WeakReference<Thread>(t);
        this.worker    = worker;
    }

    /* Create a capability, updating the global runtime state accordingly. */
    private static Capability create(final boolean worker) {
        final Capability cap = Capability.create(Thread.currentThread(), worker);
        if (worker) {
            cap.addWorker();
        } else {
            final long sequence = capabilities.write(cap);
            cap.id = capabilities.readIndex(sequence);
            capabilitiesSize.getAndIncrement();
        }
        return cap;
    }

    /* Create a capability, initializing a fresh IOManager */
    public static Capability create(final Thread t, final boolean worker) {
        Capability cap = new Capability(t, worker);
        IOManager ioManager;
        try {
            ioManager = IOManager.create(cap);
        } catch (IOException e) {
            ioManager = null;
        }
        cap.setIOManager(ioManager);
        return cap;
    }

    public void setIOManager(final IOManager ioManager) {
        this.ioManager = ioManager;
    }

    public final StgContext getContext() {
        return context;
    }

    public final int getId() {
        return id;
    }

    public final TSO getTSO() {
        return context.currentTSO;
    }

    public final Thread getThread() {
        return thread.get();
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

        schedule:
        do {
            result = null;
            pendingException = null;
            if (context.currentTSO != null) {
                /* Re-entering the RTS, a fresh TSO was generated. */
                outer = context.currentTSO;
            }

            processInbox();

            if (emptyRunQueue()) {
                switch (tryFindWork()) {
                    case SCHEDULE_CONTINUE:
                        continue schedule;
                    case SCHEDULE_RETURN:
                        return null;
                    default:
                        break;
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

    private static final int SCHEDULE_DEFAULT  = 0;
    private static final int SCHEDULE_CONTINUE = 1;
    private static final int SCHEDULE_RETURN   = 2;

    private int tryFindWork() {
        tryStealGlobalRunQueue();
        if (emptyRunQueue()) {
            activateSpark();
            if (emptyRunQueue()) {
                if (worker && workerCapabilitiesSize() >
                    Runtime.getMaxWorkerCapabilities()) {
                    /* Terminate this Worker Capability if we've exceeded the
                        limit of maxWorkerCapabilities. */
                    return SCHEDULE_RETURN;
                }

                if (Runtime.debugScheduler()) {
                    debugScheduler("Blocked!");
                }

                idleCapabilitiesSize.getAndIncrement();
                try {
                    synchronized (blockedLock) {
                        /* No condition because spurious wakeups are OK - if there is no
                           work to be done, we'll end up here again. */
                        try {
                            blockedLock.wait();
                        } catch (InterruptedException e) {

                        }
                    }
                } finally {
                    idleCapabilitiesSize.getAndDecrement();
                }
                if (Runtime.debugScheduler()) {
                    debugScheduler("Unblocked!");
                }
                return SCHEDULE_CONTINUE;
            }
        }
        return SCHEDULE_DEFAULT;
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

    public final void tryWakeupThread(final TSO tso) {
        final Capability cap = tso.cap;
        if (cap == null) {
            tso.whyBlocked = NotBlocked;
            Concurrent.pushToGlobalRunQueue(tso);
        } else if (cap != this) {
            sendMessage(tso.cap, new MessageWakeup(tso));
        } else {
            boolean blocked = true;
            boolean setNotBlocked = true;
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
                    break;
                case BlockedOnBlackHole:
                case BlockedOnSTM:
                    break;
                case BlockedOnIO:
                case BlockedOnRead:
                case BlockedOnWrite:
                case BlockedOnConnect:
                case BlockedOnAccept:
                    setNotBlocked = false;
                    break;
                default:
                    blocked = false;
                    break;
            }
            if (setNotBlocked) {
                tso.whyBlocked = NotBlocked;
            }
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
        capabilities.forEach(new Consumer<Capability>() {
                @Override
                public void accept(Capability c) {
                    c.interrupt();
                }
            });
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

    public static void shutdownCapabilities(final boolean safe) {
        int processed = 0;
        while ((processed = workerCapabilities.forEach(new Consumer<Capability>() {
                @Override
                public void accept(Capability cap) {
                    cap.shutdown(safe);
                }
            })) > 0) {
            /* Give some time for the capabilities to shut down. */
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
        blockedLoop(Runtime.getMaxTSOBlockTimeNanos());
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

        final int idleWorkers     = idleCapabilitiesSize();
        final int currentWorkSize = globalWorkSize();
        if (currentWorkSize > 0) {
            if (idleWorkers > 0) {
                unblockCapabilities(currentWorkSize);
            } else if (workerCapabilitiesSize() < Runtime.getMaxWorkerCapabilities()
                    || ((System.nanoTime() - Concurrent.globalRunQueueModifiedTime) >
                        Runtime.getMinTSOIdleTimeNanos())) {
                new WorkerThread().start();
            }
        } else  {
            /* When we have excess live threads and blocked Capabilities, let's wake
               them up so they can terminate themselves. */
            if (workerCapabilitiesSize() > Runtime.getMaxWorkerCapabilities() &&
                idleWorkers > 0) {
                unblockCapabilities(idleWorkers);
            }
        }
    }

    public static void unblockCapabilities(int n) {
        final int limit = Math.min(n, idleCapabilitiesSize());
        for (int i = 0; i < limit; i++) {
            synchronized (blockedLock) {
                blockedLock.notify();
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

    public final void registerIO(final TSO tso, final SelectableChannel channel, final int ops)
        throws IOException {
        if (ioManager == null) {
            if (Runtime.debugIO()) {
                debugIO("Selector provider not available on this platform.");
            }
        } else {
            ioManager.registerIO(this, tso, channel, ops);
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

    public void free(long address) {
        freeMessages.write(address);
    }
}
