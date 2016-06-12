package ghcvm.runtime.stg;

import ghcvm.runtime.Stg;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.StgEnter;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadBlocked;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadRunGHC;
import static ghcvm.runtime.types.StgTSO.*;

public class BlockBlackHole extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgTSO currentTSO = context.currentTSO;
        currentTSO.stack.push(new StgEnter(context.R1));
        currentTSO.whatNext = ThreadRunGHC;
        context.ret = ThreadBlocked;
        Stg.returnToSched.enter(context);
    }
}
