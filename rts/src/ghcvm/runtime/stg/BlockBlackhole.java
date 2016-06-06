package ghcvm.runtime.stg;

import ghcvm.runtime.Stg;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.types.*;
import static ghcvm.runtime.closure.StgContext.ReturnCode.*;
import static ghcvm.runtime.types.StgTSO.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;

public class BlockBlackhole extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgTSO currentTSO = context.currentTSO;
        // BLOCK_GENERIC
        // PRE_RETURN
        currentTSO.stack.push(new StgEnter(context.R1));
        currentTSO.whatNext = ThreadRunGHC;
        context.ret = ThreadBlocked;
        // returnToSched
        Stg.returnToSched.enter(context);
    }
}
