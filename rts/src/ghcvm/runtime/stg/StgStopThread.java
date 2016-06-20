package ghcvm.runtime.stg;

import java.util.Deque;
import java.util.ListIterator;

import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadComplete;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadFinished;

public class StgStopThread extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        sp.previous();
        sp.remove();
        sp.add(new StgEnter(context.R1));
        tso.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        ListIterator<StackFrame> sp = tso.sp;
        tso.whatNext = ThreadKilled;
        sp.previous();
        sp.remove();
        return false;
    }
}
