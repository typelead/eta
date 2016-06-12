package ghcvm.runtime.stg;

import ghcvm.runtime.Stg;
import ghcvm.runtime.prim.StgMVar;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.BlockReadMVarFrame;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadBlocked;

public class BlockReadMVar extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgMVar mvar = (StgMVar) context.R1;
        StgTSO tso = context.currentTSO;
        tso.stack.push(new BlockReadMVarFrame(mvar));
        tso.whatNext = ThreadRunGHC;
        context.ret = ThreadBlocked;
        Stg.returnToSched.enter(context);
    }
}
