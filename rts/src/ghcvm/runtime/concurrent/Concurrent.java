package ghcvm.runtime.concurrent;

import ghcvm.runtime.Rts;
import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.RtsFun;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.stg.StgTSO.TSO_BLOCKEX;
import static ghcvm.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static ghcvm.runtime.stg.StgTSO.TSO_LOCKED;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked;
import static ghcvm.runtime.stg.StgTSO.WhatNext;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVar;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVarRead;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadComplete;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    public static RtsFun takeMVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R(1);
                mvar.lock();
                if (mvar.value == null) {
                    StgTSO tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVar;
                    tso.inMVarOperation = true;
                    mvar.pushLast(tso);
                    context.R(1, mvar);
                    block_takemvar.enter(context);
                } else {
                    // TODO: Complete this operation
                }
                mvar.unlock();
            }
        };

    public static RtsFun readMVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R(1);
                mvar.lock();
                if (mvar.value == null) {
                    StgTSO tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVarRead;
                    tso.inMVarOperation = true;
                    mvar.pushFirst(tso);
                    context.R(1, mvar);
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
                StgMVar mvar = (StgMVar) context.R(1);
                mvar.lock();
                StgClosure val = context.R(2);
                StgTSO tso;
                if (mvar.value != null) {
                    tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVar;
                    mvar.pushLast(tso);
                    context.R(1, mvar);
                    context.R(2, val);
                    block_putmvar.enter(context);
                } else {
                    tso = mvar.popFromQueue();
                    if (tso == null) {
                        mvar.value = val;
                        // return ()
                    } else {
                        WhyBlocked whyBlocked = tso.whyBlocked;
                        // Is this pop actually necessary?
                        // TODO: Redo stack
                        // tso.stack.pop();
                        // tso.stack.push(new ReturnClosure(val));
                        tso.inMVarOperation = false;
                        context.myCapability.tryWakeupThread(tso);
                        if (whyBlocked == BlockedOnMVarRead) {
                            // TODO: check this condition if it's valid
                        }
                        // return ()
                    }
                }
                mvar.unlock();
            }
        };

    public static RtsFun block_takemvar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgMVar mvar = (StgMVar) context.R(1);
                StgTSO tso = context.currentTSO;
                // TODO: Finish this!
                //tso.sp.add(new BlockTakeMVarFrame(mvar));
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
                StgMVar mvar = (StgMVar) context.R(1);
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
                StgMVar mvar = (StgMVar) context.R(1);
                StgClosure val = context.R(2);
                StgTSO tso = context.currentTSO;
                tso.stack.push(new BlockPutMVarFrame(mvar, val));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };

    public static RtsFun tryReadMVar = new RtsFun() {
        @Override
        public void enter(StgContext context) {
            StgMVar mvar = (StgMVar) context.R(1);
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
                context.O(1, tso);
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
                context.O(1, tso);
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
                context.O(1, tso);
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
                StgTSO tso = (StgTSO) context.O(1);
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
