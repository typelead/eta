package ghcvm.runtime.concurrent;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVar;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnMVarRead;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

public class Concurrent {
    public static final int SPIN_COUNT = 1000;

    public static StgClosure readMVar = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R1;
                mvar.lock();
                if (mvar.value == null) {
                    StgTSO tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVarRead;
                    tso.inMVarOperation = true;
                    mvar.pushFirst(tso);
                    context.R1 = mvar;
                    block_readmvar.enter(context);
                } else {
                    context.R1 = mvar.value;
                }
                mvar.unlock();
            }
        };

    public static StgClosure putMVar = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R1;
                mvar.lock();
                StgClosure val = context.R2;
                StgTSO tso;
                if (mvar.value != null) {
                    tso = context.currentTSO;
                    tso.blockInfo = mvar;
                    tso.whyBlocked = BlockedOnMVar;
                    mvar.pushLast(tso);
                    context.R1 = mvar;
                    context.R2 = val;
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

    public static StgClosure block_readmvar = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R1;
                StgTSO tso = context.currentTSO;
                tso.sp.add(new BlockReadMVarFrame(mvar));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };

    public static StgClosure block_putmvar = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgMVar mvar = (StgMVar) context.R1;
                StgClosure val = context.R2;
                StgTSO tso = context.currentTSO;
                tso.stack.push(new BlockPutMVarFrame(mvar, val));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };

    public static StgClosure tryReadMVar = new StgClosure() {
        @Override
        public void enter(StgContext context) {
            StgMVar mvar = (StgMVar) context.R1;
            mvar.lock();
            StgClosure value = mvar.value;
            if (value == null) {
                context.I1 = 0;
                context.R1 = null;
            } else {
                context.I1 = 1;
                context.R1 = value;
            }
            mvar.unlock();
        }
    };

}
