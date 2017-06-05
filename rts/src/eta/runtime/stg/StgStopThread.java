package eta.runtime.stg;

import java.util.Deque;
import java.util.Stack;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.thunk.StgThunk;
import eta.runtime.exception.StgException;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadComplete;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadFinished;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Stop;

public class StgStopThread extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        StgClosure ret = context.R(1);
        sp.previous();
        sp.remove();
        sp.add(new StgEnter(ret));
        tso.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        tso.whatNext = ThreadKilled;
        tso.sp.remove();
        return false;
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, StgTSO tso, StgClosure exception) {
        tso.stack.clear();
        LinkedList<StackFrame> stack = new LinkedList<StackFrame>();
        tso.stack = stack;
        tso.sp = stack.listIterator();
        tso.spPush(new StgEnter(exception));
        tso.whatNext = ThreadKilled;
        Stg.threadFinished(context);
        return false;
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) { return Stop; }
}
