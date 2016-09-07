package ghcvm.runtime.stg;

import java.util.Stack;
import java.util.Deque;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.List;
import java.util.ListIterator;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicBoolean;

import ghcvm.runtime.*;
import ghcvm.runtime.stg.*;
import ghcvm.runtime.message.*;
import ghcvm.runtime.concurrent.*;
import ghcvm.runtime.stm.*;
import ghcvm.runtime.stg.*;
import ghcvm.runtime.stg.Task.InCall;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.*;
import static ghcvm.runtime.stg.StgTSO.WhatNext.*;
import static ghcvm.runtime.concurrent.Concurrent.SPIN_COUNT;
import static ghcvm.runtime.RtsMessages.barf;

public final class StgTSO extends StgClosure {
    public static AtomicLong maxThreadId = new AtomicLong(0);
    public long id = nextThreadId();
    public volatile StgTSO link;
    public Stack<StackFrame> stack = new Stack<StackFrame>();
    public ListIterator<StackFrame> sp;
    public Queue<StgBlockingQueue> blockingQueues = new ArrayDeque<StgBlockingQueue>();
    public WhatNext whatNext = ThreadRunGHC;
    public WhyBlocked whyBlocked = NotBlocked;
    public Task.InCall bound;
    public StgTRecHeader trec;
    public Capability cap;
    public StgClosure blockInfo;
    public int flags;
    public long wakeTime = -1;
    public boolean inMVarOperation;
    public Deque<MessageThrowTo> blockedExceptions = new ArrayDeque<MessageThrowTo>();
    public AtomicBoolean lock = new AtomicBoolean(false);

    /* TSO Flags */
    public static final int TSO_LOCKED = 2;
    public static final int TSO_BLOCKEX = 4;
    public static final int TSO_INTERRUPTIBLE = 8;
    public static final int TSO_STOPPED_ON_BREAKPOINT = 16;
    public static final int TSO_MARKED = 64;
    public static final int TSO_SQUEEZED = 128;
    public static final int  TSO_ALLOC_LIMIT = 256;

    public enum WhatNext {
        ThreadRunGHC,
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

    public StgTSO(Capability cap) {
        this.cap = cap;
        this.sp = stack.listIterator();
        pushClosure(new StgStopThread());
    }

    public void pushClosure(StackFrame frame) {
        sp.add(frame);
    }

    public static long nextThreadId() {
        return maxThreadId.getAndIncrement();
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

    public final void removeFromMVarBlockedQueue() {
        StgMVar mvar = (StgMVar) blockInfo;
        if (!inMVarOperation) return;
        mvar.tsoQueue.remove(this);
        inMVarOperation = false;
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

    public final void removeFlags(int flags) {
        this.flags &= ~flags;
    }

    public final void addFlags(int flags) {
        this.flags |= flags;
    }

    @Override
    public final boolean blackHole(Capability cap, MessageBlackHole msg) {
        if (this.cap != cap) {
            cap.sendMessage(this.cap, msg);
        } else {
            StgBlockingQueue bq = new StgBlockingQueue(this, msg);
            blockingQueues.offer(bq);
            if (whyBlocked == NotBlocked && id != msg.tso.id) {
                cap.promoteInRunQueue(this);
            }
            msg.bh.indirectee = bq;
        }
        return false;
    }

    @Override
    public final void thunkUpdate(Capability cap, StgTSO tso) {
        if (tso != this) {
            cap.checkBlockingQueues(tso);
        }
    }

    public final void lock() {
        do {
            int i = 0;
            do {
                boolean old = lock.getAndSet(true);
                if (!old) return;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
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

    public void park() {
        whyBlocked = BlockedOnSTM;
        blockInfo = null;
    }

    public final boolean isBound() {
        if (RtsFlags.ModeFlags.threaded) {
            return bound != null;
        } else {
            return false;
        }
    }

    // Stack operations
    public final void spPrevious() {
        sp.previous();
    }

    public final void spNext() {
        sp.next();
    }

    public final void spRemove() {
        sp.remove();
    }

    public final void spPush(StackFrame frame) {
        sp.add(frame);
    }

    public final void spPop() {
        sp.previous();
        sp.remove();
    }

    @Override
    public void enter(StgContext context) {
        barf("TSO object entered!");
    }

    @Override
    public StgClosure getEvaluated() { return this; }
}
