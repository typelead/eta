package ghcvm.runtime.types;

import java.util.Deque;
import java.util.ArrayDeque;
import java.util.List;
import java.util.ArrayList;

import java.util.concurrent.atomic.AtomicLong;


import ghcvm.runtime.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.message.*;
import static ghcvm.runtime.types.Task.InCall;

public class StgTSO extends StgClosure {
    public static AtomicLong maxThreadId = new AtomicLong(0);
    public long id;
    public volatile StgTSO link;
    //    public StgTSO globalLink; This field may not be necessary
    public Deque<StackFrame> stack;
    public WhatNext whatNext;
    public WhyBlocked whyBlocked;
    public InCall bound;
    // public StgTRecHeader trec; deal with later when we implement STM
    public Capability cap;
    public BlockInfo blockInfo;
    public static class BlockInfo {
        StgClosure closure;
        public BlockInfo(StgClosure closure) {
            setInfo(closure);
        }
        public void setInfo(StgClosure closure) {
            this.closure = closure;
        }
    }
    public int flags;
    public int savedErrno;
    public Deque<MessageThrowTo> blockedExceptions = new ArrayDeque<MessageThrowTo>();
    // public StgBlockingQueue bq;
    // If PROFILING StgTSOProfInfo prof;

    // Flags
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
        NotBlocked,
        BlockedOnMVar,
        BlockedOnBlackHole,
        BlockedOnRead,
        BlockedOnWrite,
        BlockedOnDelay,
        BlockedOnSTM,
        BlockedOnDoProc,
        BlockedOnGA,
        BlockedOnCCall,
        BlockedOnCCall_Interruptible,
        BlockedOnMsgThrowTo,
        ThreadMigrating,
        BlockedOnMVarRead
    }

    public StgTSO(Capability cap) {
        this.whatNext = WhatNext.ThreadRunGHC;
        this.whyBlocked = WhyBlocked.NotBlocked;
        this.cap = cap;
        this.id = nextThreadId();
        this.stack = new ArrayDeque<StackFrame>(1);
        pushClosure(new StgStopThread());
    }

    public void pushClosure(StackFrame frame) {
        stack.push(frame);
    }

    public static long nextThreadId() {
        return maxThreadId.getAndIncrement();
    }

    public boolean interruptible() {
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

    public void removeFromMVarBlockedQueue() {
        // TODO: Implement
    }
}
