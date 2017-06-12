package eta.runtime.concurrent;

import eta.runtime.Runtime;
import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.ReturnClosure;
import eta.runtime.exception.Exception;
import static eta.runtime.RuntimeLogging.barf;
import static eta.runtime.stg.TSO.TSO_BLOCKEX;
import static eta.runtime.stg.TSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.TSO.TSO_LOCKED;
import static eta.runtime.stg.TSO.WhyBlocked;
import static eta.runtime.stg.TSO.WhatNext;
import static eta.runtime.stg.TSO.WhyBlocked.BlockedOnMVar;
import static eta.runtime.stg.TSO.WhyBlocked.BlockedOnMVarRead;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;
import static eta.runtime.stg.TSO.WhatNext.ThreadComplete;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    public static final Deque<TSO> globalRunQueue = new ConcurrentLinkedDeque<TSO>();

    public static void pushToGlobalRunQueue(TSO tso) {
        assert tso.cap == null;
        globalRunQueue.offerFirst(tso);
    }

    public static Closure takeMVar(StgContext context, MVar mvar) {
        do {
            try {
                return mvar.take();
            } catch (InterruptedException ie) {}
        } while (true);
    }

    public static Closure readMVar(StgContext context, MVar mvar) {
        return mvar.read();
    }

    public static Closure putMVar(StgContext context, MVar mvar, Closure val) {
        do {
            try {
                mvar.put(val);
                return null;
            } catch (InterruptedException ie) {}
        } while (true);
    }

    public static Closure tryReadMVar(StgContext context, MVar mvar) {
        Closure value = mvar.tryRead();
        context.I(1, (value == null)? 0: 1);
        return value;
    }

    /* TODO: Perform blackholing here to prevent duplicate evaluations
             shared among multiple threads? */
    public static Closure fork(StgContext context, Closure closure) {
        TSO currentTSO = context.currentTSO;
        TSO tso = Rts.createIOThread(null, closure);
        tso.addFlags(currentTSO.andFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE));
        Capability.pushToGlobalRunQueue(tso);
        context.O(1, tso);
        return null;
    }

    /* TODO: The scheduling policy in Eta is for TSOs to get grabbed by Capabilities
             and keep executing them to completion (which rarely happens).

             Hence, it makes no sense to use `forkOn` in Eta since threads will
             be bound to a given thread anyways. If you put multiple threads on
             a single Capability, be warned that one of the threads may never run!
     */
    public static Closure forkOn(StgContext context, int cpu, Closure closure) {
        Capability cap = context.myCapability;
        TSO tso = Rts.createIOThread(cap, closure);
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        Rts.scheduleThreadOn(cap, cpu, tso);
        context.O(1, tso);
        return null;
    }

    public static Closure yield(StgContext context) {
        cap.blockedLoop(true);
        Thread.yield();
        return null;
    }

    /* TODO: Inline this */
    public static Closure isCurrentThreadBound(StgContext context) {
        context.I(1, context.currentTSO.isBound()? 1 : 0);
        return null;
    }

    public static Closure threadStatus(StgContext context, TSO tso) {
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
        int cap = tso.cap.id;
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
