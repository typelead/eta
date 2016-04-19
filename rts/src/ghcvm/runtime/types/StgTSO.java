package ghcvm.runtime.types;

import java.util.LinkedList;
import ghcvm.runtime.*;
import static ghcvm.runtime.types.Task.InCall;
import ghcvm.runtime.closure.*;

public class StgTSO {
    public static int nextThreadId = 0;
    public int id; // Should this be long instead?
    public volatile StgTSO link;
    //    public StgTSO globalLink; This filed may not be necessary
    public StgClosure stackBottom;
    public StgClosure stackTop;
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

    public enum ReturnCode {
        HeapOverflow,
        StackOverflow,
        ThreadYielding,
        ThreadBlocked,
        ThreadFinished
    }

    public StgTSO(Capability cap) {
        this.whatNext = WhatNext.ThreadRunGHC;
        this.whyBlocked = WhyBlocked.NotBlocked;
        this.cap = cap;
        // TODO: Should this synchronized block be placed outside?
        synchronized (RtsScheduler.class) {
            this.id = nextThreadId++;
        }
    }

    public void pushClosure(StgClosure c) {
    }
}
