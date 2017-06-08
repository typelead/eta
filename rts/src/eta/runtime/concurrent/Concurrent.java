package eta.runtime.concurrent;

import eta.runtime.Rts;
import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Closure;
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
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRun;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadComplete;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    public static Closure takeMVar(StgContext context, StgMVar mvar) {
        do {
            try {
                return mvar.take();
            } catch (InterruptedException ie) {}
        } while (true);
    }

    public static Closure readMVar(StgContext context, StgMVar mvar) {
        return mvar.read();
    }

    public static Closure putMVar(StgContext context, StgMVar mvar, Closure val) {
        do {
            try {
                mvar.put(val);
                return null;
            } catch (InterruptedException ie) {}
        } while (true);
    }

    public static Closure tryReadMVar(StgContext context, StgMVar mvar) {
        Closure value = mvar.tryRead();
        context.I(1, (value == null)? 0: 1);
        return value;
    }

    /* TODO: Perform blackholing here to prevent duplicate evaluations
             shared among multiple threads? */
    public static Closure fork(StgContext context, Closure closure) {
        Capability cap = context.myCapability;
        StgTSO tso = Rts.scheduleIOClosure(closure);
        cap.contextSwitch = true;
        context.O(1, tso);
        return null;
    }

    public static Closure forkOn(StgContext context, int cpu, Closure closure) {
        Capability cap = context.myCapability;
        StgTSO tso = Rts.createIOThread(cap, closure);
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        Rts.scheduleThreadOn(cap, cpu, tso);
        cap.contextSwitch = true;
        context.O(1, tso);
        return null;
    }

    public static Closure yield(StgContext context) {
        Thread.yield();
        return null;
    }

    /* TODO: Inline this */
    public static Closure isCurrentThreadBound(StgContext context) {
        context.I(1, context.currentTSO.isBound()? 1 : 0);
        return null;
    }

    public static Closure threadStatus(StgContext context, StgTSO tso) {
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
        return null;
    }

    /* TODO: Implement this */
    public static Closure traceEvent(StgContext context) {
        return null;
    }
}
