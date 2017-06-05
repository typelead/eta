package eta.runtime.concurrent;

import eta.runtime.Rts;
import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.ReturnClosure;
import eta.runtime.exception.StgException;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.StgTSO.TSO_LOCKED;
import static eta.runtime.stg.StgTSO.WhyBlocked;
import static eta.runtime.stg.StgTSO.WhatNext;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVar;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVarRead;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadComplete;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    public static void takeMVar(StgContext context, StgMVar mvar) {
        try {
            context.R(1, mvar.take());
        } catch (InterruptedException ie) {
            barf("takeMVar: Unexpected interrupt.");
        }
    }

    public static void readMVar(StgContext context, StgMVar mvar) {
        context.R(1, mvar.read());
    }

    public static void putMVar(StgContext context, StgMVar mvar, StgClosure val) {
        try {
            mvar.put(val);
        } catch (InterruptedException ie) {
            barf("putMVar: Unexpected interrupt.");
        }
    }

    public static void tryReadMVar(StgContext context, StgMVar mvar) {
        StgClosure value = mvar.tryRead();
        context.I(1, (value == null)? 0: 1);
        context.R(1, value);
    }

    public static void fork(StgContext context, StgClosure closure) {
        Capability cap = context.myCapability;
        StgTSO tso = Rts.scheduleIOClosure(closure);
        cap.contextSwitch = true;
        context.O(1, tso);
    }

    public static void forkOn(StgContext context, int cpu, StgClosure closure) {
        Capability cap = context.myCapability;
        StgTSO tso = Rts.createIOThread(cap, closure);
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        Rts.scheduleThreadOn(cap, cpu, tso);
        cap.contextSwitch = true;
        context.O(1, tso);
    }

    public static void yield(StgContext context) {
        Thread.yield();
    }

    /* TODO: Inline this */
    public static void isCurrentThreadBound(StgContext context) {
        StgTSO tso = context.currentTSO;
        context.I(1, tso.isBound()? 1 : 0);
    }

    public static void threadStatus(StgContext context, StgTSO tso) {
        WhatNext whatNext = tso.whatNext;
        int ret;
        WhyBlocked whyBlocked = tso.whyBlocked;
        if (whatNext == ThreadComplete) {
            ret = 16;
        } else {
            if (whatNext == ThreadKilled) {
                ret = 17;
            } else {
                ret = whyBlocked.getVal();
            }
        }
        int cap = tso.cap.no;
        int locked;
        if (tso.hasFlag(TSO_LOCKED)) {
            locked = 1;
        } else {
            locked = 0;
        }
        context.I(1, ret);
        context.I(2, cap);
        context.I(3, locked);
    }

    /* TODO: Implement this */
    public static void traceEvent(StgContext context) {}
}
