package eta.runtime.stg;

import java.util.Stack;
import java.util.Deque;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.*;
import eta.runtime.stg.*;
import eta.runtime.message.*;
import eta.runtime.concurrent.*;
import eta.runtime.stm.*;
import eta.runtime.stg.*;
import eta.runtime.stg.Task.InCall;
import eta.runtime.thunk.Thunk;
import static eta.runtime.stg.TSO.WhyBlocked.*;
import static eta.runtime.stg.TSO.WhatNext.*;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.RuntimeLogging.debugBelch;

public final class TSO extends BlackHole {
    public static AtomicInteger maxThreadId = new AtomicInteger(0);
    public int id = nextThreadId();
    public UpdateInfoStack updateInfoStack = new UpdateInfoStack();
    public Queue<BlockingQueue> blockingQueues = new LinkedList<BlockingQueue>();
    public WhatNext whatNext = ThreadRun;
    public WhyBlocked whyBlocked = NotBlocked;
    public TransactionRecord trec;
    public Capability cap;
    public Object blockInfo;
    public int flags;
    public Queue<MessageThrowTo> blockedExceptions
        = new ConcurrentLinkedQueue<ThrowTo>();
    public StackTraceElement[] stackTrace;
    public AtomicBoolean lock = new AtomicBoolean(false);

    /* DEPRECATED */
    public LinkedList<StackFrame> stack;
    public ListIterator<StackFrame> sp;
    public long wakeTime = -1;
    public boolean inMVarOperation;

    /* TSO Flags */
    public static final int TSO_LOCKED = 2;
    public static final int TSO_BLOCKEX = 4;
    public static final int TSO_INTERRUPTIBLE = 8;
    public static final int TSO_MARKED = 64;
    public static final int TSO_SQUEEZED = 128;
    public static final int  TSO_ALLOC_LIMIT = 256;

    public enum WhatNext {
        ThreadRun,
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
        BlockedOnGA(8),
        BlockedOnJavaCall(10),
        BlockedOnJavaCall_Interruptible(11),
        BlockedOnMsgThrowTo(12),
        ThreadMigrating(13),
        BlockedOnMVarRead(14);
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

    public void pushClosure(StackFrame frame) {
        sp.add(frame);
    }

    public static int nextThreadId() {
        return maxThreadId.getAndIncrement();
    }

    public static int getThreadId(TSO tso) {
        return tso.id;
    }

    public final boolean interruptible() {
        switch (whyBlocked) {
            case BlockedOnMVar:
            case BlockedOnSTM:
            case BlockedOnMVarRead:
            case BlockedOnMsgThrowTo:
            case BlockedOnRead:
            case BlockedOnWrite:
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
            cap.throwToSingleThreaded(this, null);
        }
    }

    public final void handleThreadBlocked() {
        // debug output
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
        return lock.getAndSet(true);
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
        if (whyBlocked == BlockedOnSTM &&
            blockInfo == STM.awake) {
            /* Already woken up */
        } else if (whyBlocked == BlockedOnSTM) {
            blockInfo = STM.awake;
            cap.tryWakeupThread(this);
        } else {
            /* Spurious unpark */
        }
        unlock();
    }

    public final boolean isBound() {
        if (RuntimeOptions.ModeFlags.threaded) {
            return bound != null;
        } else {
            return false;
        }
    }

    // Stack operations
    public final void spPrevious() {
        barf("spPrevious");
        sp.previous();
    }

    public final void spNext() {
        barf("spNext");
        sp.next();
    }

    public final void spRemove() {
        barf("spRemove");
        sp.remove();
    }

    public final void spPush(StackFrame frame) {
        barf("spPush");
        sp.add(frame);
    }

    public final StackFrame spPop() {
        barf("spPop");
        StackFrame frame = sp.previous();
        sp.remove();
        return frame;
    }

    @Override
    public final Closure enter(StgContext context) {
        barf("TSO object entered!");
        return null;
    }

    public final void dump() {
        System.out.println("TSO #" + id);
        if (sp.hasPrevious()) {
            System.out.println("Sp = " + sp.previous());
            sp.next();
        }
        ListIterator<StackFrame> it = stack.listIterator();
        int i = 0;
        while (it.hasNext()) {
            System.out.println("#" + i + ": " + it.next());
            i++;
        }
    }

    public final void setStackTrace(StackTraceElement[] stackTrace) {
        this.stackTrace = stackTrace;
    }

    public final StackTraceElement[] getStackTrace() {
        return this.stackTrace;
    }

    public final boolean hasStackTrace() {
        return this.stackTrace != null;
    }

    public final void removeFromQueues() {
        switch (whyBlocked) {
            case NotBlocked:
            case ThreadMigrating:
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
                // TODO: Remove the following check if this state never occurs
                // if the threaded rts
                if (RuntimeOptions.ModeFlags.threaded) {
                    blockedQueue.remove(tso);
                }
                break;
            case BlockedOnDelay:
                sleepingQueue.remove(tso);
                break;
            default:
                barf("removeFromQueues: %d", tso.whyBlocked);
        }
        whyBlocked = NotBlocked;
    }

    public final void blockedThrowTo(MessageThrowTo msg) {
        blockedExceptions.offer(msg);
    }
}
