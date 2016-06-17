package ghcvm.runtime.stackframe;

import java.util.Deque;
import java.util.ListIterator;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.types.StgTSO.WhatNext.ThreadComplete;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadFinished;

public class StgStopThread extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO currentTSO = context.currentTSO;
        ListIterator<StackFrame> sp = context.sp;
        sp.remove();
        sp.add(new StgEnter(context.R1));
        currentTSO.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }
}
