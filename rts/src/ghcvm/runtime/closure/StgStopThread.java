package ghcvm.runtime.closure;

import ghcvm.runtime.types.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;
import static ghcvm.runtime.types.StgTSO.ReturnCode.*;

public class StgStopThread extends StackFrame {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgTSO currentTSO = context.currentTSO;
        currentTSO.stack.pop();
        // TODO: Should a stub enter frame be generated?
        currentTSO.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
    }
}
