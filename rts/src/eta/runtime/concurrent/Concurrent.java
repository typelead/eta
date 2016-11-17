package eta.runtime.concurrent;

import eta.runtime.Rts;
import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.RtsFun;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.ReturnClosure;
import eta.runtime.exception.StgException;
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

    public static RtsFun takeMVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.O(1);
                StgTSO tso;
                mvar.lock();
                if (mvar.value == null) {
                    tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVar;
                    tso.inMVarOperation = true;
                    mvar.pushLast(tso);
                    context.O(1, mvar);
                    block_takemvar.enter(context);
                } else {
                    StgClosure val = mvar.value;
                    if (mvar.isEmpty()) {
                        mvar.value = null;
                        mvar.unlock();
                        context.R(1, val);
                    } else {
                        tso = mvar.popFromQueue();
                        Capability cap = context.myCapability;
                        BlockPutMVarFrame frame = (BlockPutMVarFrame) tso.spPop();
                        mvar.value = frame.val;
                        tso.inMVarOperation = false;
                        cap.tryWakeupThread(tso);
                        mvar.unlock();
                        context.R(1, val);
                    }
                }
            }
        };

    public static RtsFun readMVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.O(1);
                mvar.lock();
                if (mvar.value == null) {
                    StgTSO tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVarRead;
                    tso.inMVarOperation = true;
                    mvar.pushFirst(tso);
                    context.O(1, mvar);
                    block_readmvar.enter(context);
                } else {
                    context.R(1, mvar.value);
                }
                mvar.unlock();
            }
        };

    public static RtsFun putMVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.O(1);
                StgClosure val = context.R(1);
                mvar.lock();
                StgTSO tso;
                if (mvar.value != null) {
                    tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVar;
                    tso.inMVarOperation = true;
                    mvar.pushLast(tso);
                    context.O(1, mvar);
                    context.R(1, val);
                    block_putmvar.enter(context);
                } else {
                    boolean loop;
                    do {
                        loop = false;
                        tso = mvar.popFromQueue();
                        if (tso == null) {
                            mvar.value = val;
                            mvar.unlock();
                            context.R(1, null); // Is this necessary?
                        } else {
                            Capability cap = context.myCapability;
                            WhyBlocked whyBlocked = tso.whyBlocked;
                            tso.spPop();
                            tso.spPush(new ReturnClosure(val));
                            tso.inMVarOperation = false;
                            context.myCapability.tryWakeupThread(tso);
                            if (whyBlocked == BlockedOnMVarRead) {
                                loop = true;
                            } else {
                                mvar.unlock();
                                context.R(1, null); // Is this necessary?
                            }
                        }
                    } while (loop);
                }
            }
        };

    public static RtsFun block_takemvar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgMVar mvar = (StgMVar) context.O(1);
                StgTSO tso = context.currentTSO;
                tso.sp.add(new BlockTakeMVarFrame(mvar));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                cap.threadPaused(tso);
                mvar.unlock();
                throw StgException.stgReturnException;
            }
        };

    public static RtsFun block_readmvar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.O(1);
                StgTSO tso = context.currentTSO;
                tso.sp.add(new BlockReadMVarFrame(mvar));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };

    public static RtsFun block_putmvar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                StgMVar mvar = (StgMVar) context.O(1);
                StgClosure val = context.R(1);
                tso.spPush(new BlockPutMVarFrame(mvar, val));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                cap.threadPaused(tso);
                mvar.unlock();
                throw StgException.stgReturnException;
            }
        };

    public static RtsFun tryReadMVar = new RtsFun() {
        @Override
        public void enter(StgContext context) {
            StgMVar mvar = (StgMVar) context.O(1);
            mvar.lock();
            StgClosure value = mvar.value;
            if (value == null) {
                context.I(1, 0);
                /* TODO: Verify that null is an appropriate value to
                         return */
                context.R(1, null);
            } else {
                context.I(1, 1);
                context.R(1, value);
            }
            mvar.unlock();
        }
    };

    public static RtsFun fork = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO currentTSO = context.currentTSO;
                StgClosure closure = context.R(1);
                StgTSO tso = Rts.createIOThread(cap, closure);
                tso.addFlags(currentTSO.flags & (TSO_BLOCKEX | TSO_INTERRUPTIBLE));
                Rts.scheduleThread(cap, tso);
                cap.contextSwitch = true;
                context.R(1, tso);
            }
        };

    public static RtsFun forkOn = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO currentTSO = context.currentTSO;
                int cpu = context.I(1);
                StgClosure closure = context.R(1);
                StgTSO tso = Rts.createIOThread(cap, closure);
                tso.addFlags(currentTSO.flags & (TSO_BLOCKEX | TSO_INTERRUPTIBLE));
                Rts.scheduleThreadOn(cap, cpu, tso);
                cap.contextSwitch = true;
                context.R(1, tso);
            }
        };

    public static RtsFun yield = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                cap.contextSwitch = true;
                Concurrent.yield_noregs.enter(context);
            }
        };

    public static RtsFun yield_noregs = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.ret = ThreadYielding;
                tso.whatNext = ThreadRunGHC;
                Stg.returnToSched.enter(context);
            }
        };

    public static RtsFun myThreadId = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.R(1, tso);
            }
        };

    public static RtsFun isCurrentThreadBound = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.I(1, tso.isBound()? 1 : 0);
            }
        };

    public static RtsFun threadStatus = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = (StgTSO) context.R(1);
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
        };

}
