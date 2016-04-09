package ghcvm.runtime;

import java.util.ArrayList;
// import static ghcvm.runtime.Task;

public class StgTSO {
    public int threadId; // Should this be long instead?
    public StgTSO link;
    //    public StgTSO globalLink; This filed may not be necessary
    public ArrayList<StackFrame> stack;
    public WhatNext whatNext;
    public WhyBlocked whyBlocked;
    public InCall bound;
    // public StgTRecHeader trec; deal with later when we implement STM
    public Capability cap;
    public Object blockInfo;
    public int flags;
    public int savedErrno;
    // public List<MessageThrowTo> blockedExceptions;
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
        ThreadRunGHC, ThreadInterpret, ThreadKilled, ThreadComplete
    }

    public enum WhyBlocked {
        NotBlocked,
        BlockedOnMVar,
        BlockedOnMVarRead,
        BlockedOnBlackHole,
        BlockedOnRead,
        BlockedOnWrite,
        BlockedOnDelay,
        BlockedOnSTM
    }

    public enum ReturnCode {
        HeapOverflow,
        StackOverflow,
        ThreadYielding,
        ThreadBlocked,
        ThreadFinished
    }

}
