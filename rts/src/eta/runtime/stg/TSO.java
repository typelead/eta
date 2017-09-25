package eta.runtime.stg;

import java.util.Stack;
import java.util.Deque;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.ArrayList;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.exception.Exception;
import eta.runtime.message.MessageThrowTo;
import eta.runtime.stm.STM;
import eta.runtime.stm.TransactionRecord;
import eta.runtime.thunk.BlackHole;
import eta.runtime.thunk.UpdateInfoStack;
import eta.runtime.thunk.BlockingQueue;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugBelch;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public final class TSO extends BlackHole {
    public static AtomicInteger maxThreadId = new AtomicInteger();
    public int id = maxThreadId.getAndIncrement();
    public Closure closure;
    public UpdateInfoStack updateInfoStack = new UpdateInfoStack();
    public Queue<BlockingQueue> blockingQueues = new LinkedList<BlockingQueue>();
    public WhatNext whatNext = ThreadRun;
    public WhyBlocked whyBlocked = NotBlocked;
    public TransactionRecord trec;
    public Capability cap;
    public Object blockInfo;
    public int flags;
    public Queue<MessageThrowTo> blockedExceptions
        = new ConcurrentLinkedQueue<MessageThrowTo>();
    public StackTraceElement[] stackTrace;
    public Throwable cause;
    public AtomicBoolean lock = new AtomicBoolean(false);

    /* TSO Flags */
    public static final int TSO_LOCKED = 2;
    public static final int TSO_BLOCKEX = 4;
    public static final int TSO_INTERRUPTIBLE = 8;
    public static final int TSO_INTERRUPT_IMMUNE = 64;

    public enum WhatNext {
        ThreadRun,
        ThreadYield,
        ThreadBlock,
        ThreadInterpret,
        ThreadKilled,
        ThreadComplete
    }

    public enum WhyBlocked {
        NotBlocked(0),
        BlockedOnMVar(1),
        BlockedOnBlackHole(2),
        BlockedOnRead(3),
        BlockedOnWrite(4),
        BlockedOnDelay(5),
        BlockedOnSTM(6),
        BlockedOnFuture(8),
        BlockedOnIO(9),
        BlockedOnJavaCall(10),
        BlockedOnJavaCall_Interruptible(11),
        BlockedOnMsgThrowTo(12),
        BlockedOnMVarRead(14),
        BlockedOnYield(15),
        BlockedOnConnect(16),
        BlockedOnAccept(17);
        private int val;
        WhyBlocked(int val) {
            this.val = val;
        }
        public int getVal() {
            return val;
        }
    }

    public TSO(Closure closure) {
        this.closure = closure;
    }

    public static int getThreadId(TSO tso) {
        return tso.id;
    }

    public final boolean interruptible() {
        switch (whyBlocked) {
            case BlockedOnMVar:
        case BlockedOnMVarRead:
            case BlockedOnSTM:
            case BlockedOnMsgThrowTo:
            case BlockedOnRead:
            case BlockedOnWrite:
            case BlockedOnFuture:
            case BlockedOnDelay:
                return true;
            default:
                return false;
        }
    }

    public final boolean isFlagLocked() { return hasFlag(TSO_LOCKED); }

    public final void delete() {
        if (whyBlocked != BlockedOnJavaCall &&
            whyBlocked != BlockedOnJavaCall_Interruptible) {
            Exception.throwToSingleThreaded(this, null);
        }
    }

    public final boolean hasFlag(int flag) {
        return ((flags & flag) != 0);
    }

    public final int andFlags(int flag) {
        return flags & flag;
    }

    public final void removeFlags(int flags) {
        this.flags &= ~flags;
    }

    public final void addFlags(int flags) {
        this.flags |= flags;
    }

    public final void lock() {
        while (!lock.compareAndSet(false, true)) {
            /* TODO: Add some sort of yielding here? */
        }
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.compareAndSet(false, true);
    }

    public final int showIfFlags(int flags) {
        return this.flags & flags;
    }

    public final void park() {
        assert whyBlocked == NotBlocked;
        whyBlocked = BlockedOnSTM;
        blockInfo = null;
    }

    public final void unpark(Capability cap) {
        lock();
        if (whyBlocked == BlockedOnSTM && blockInfo == null) {
            blockInfo = STM.awake;
            cap.tryWakeupThread(this);
        }
        unlock();
    }

    public final StackTraceElement[] getStackTrace() {
        return this.stackTrace;
    }

    public final void setStackTrace(StackTraceElement[] stackTrace) {
        this.stackTrace = stackTrace;
    }

    public final Throwable getCause() {
        return this.cause;
    }

    public final void setCause(Throwable cause) {
        this.cause = cause;
    }

    public final void saveStack(Throwable t, StackTraceElement[] stes) {
        if (this.cause != null) {
            this.cause.setStackTrace(this.stackTrace);
            t.initCause(this.cause);
        }
        this.cause = t;
        this.stackTrace = stes;
    }

    public final void resetStack() {
        this.cause      = null;
        this.stackTrace = null;
    }

    public final boolean hasStackTrace() {
        return this.stackTrace != null;
    }

    public final void removeFromQueues() {
        switch (whyBlocked) {
            case NotBlocked:
                return;
            case BlockedOnSTM:
                /* Check for zombie transactions */
                break;
            case BlockedOnMVar:
            case BlockedOnMVarRead:
                /* TODO: Figure out MVar story */
                break;
            case BlockedOnBlackHole:
                break;
            case BlockedOnMsgThrowTo:
                MessageThrowTo m = (MessageThrowTo) blockInfo;
                m.done();
                break;
            case BlockedOnRead:
            case BlockedOnWrite:
            case BlockedOnDelay:
                break;
            default:
                barf("removeFromQueues: %d", whyBlocked);
        }
        whyBlocked = NotBlocked;
    }

    public final void blockedThrowTo(MessageThrowTo msg) {
        blockedExceptions.offer(msg);
    }

    public final void interrupt() {
        if (cap != null) {
            cap.interrupt();
        }
    }

    /* Preserves the enclosing interrupt status. */
    public final boolean suspendInterrupts(boolean interruptible) {
        boolean immune = hasFlag(TSO_INTERRUPT_IMMUNE);
        if (interruptible) {
            whyBlocked = BlockedOnJavaCall_Interruptible;
        } else {
            if (!immune) {
                addFlags(TSO_INTERRUPT_IMMUNE);
                // Erase interrupt status
                Thread.interrupted();
            }
            whyBlocked = BlockedOnJavaCall;
        }
        cap.idleLoop(true);
        return immune;
    }

    public final void resumeInterrupts(boolean immune) {
        if (!immune) removeFlags(TSO_INTERRUPT_IMMUNE);
        else addFlags(TSO_INTERRUPT_IMMUNE);
        cap.idleLoop(false);
        whyBlocked = NotBlocked;
    }

    public final void setName(String name) {
        if (cap != null) {
            Thread t = cap.thread.get();
            if (t != null) {
                t.setName(name);
            }
        }
    }

    public final void reset() {
        updateInfoStack.clear();
    }
}
