package ghcvm.runtime.closure;

import java.util.Deque;
import ghcvm.runtime.types.*;
import static ghcvm.runtime.types.StgTSO.WhatNext.*;
import static ghcvm.runtime.closure.StgContext.ReturnCode.*;

public class StgStopThread extends StackFrame {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgTSO currentTSO = context.currentTSO;
        Deque<StackFrame> stack = currentTSO.stack;
        stack.pop();
        stack.push(new StgEnter(context.R1));
        currentTSO.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
    }
}
