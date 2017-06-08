package eta.runtime.stg;

import java.util.Deque;
import java.util.Stack;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.thunk.Thunk;
import eta.runtime.exception.StgException;
import static eta.runtime.stg.TSO.WhatNext.ThreadComplete;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadFinished;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Stop;

public class StgStopThread extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        TSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        Closure ret = context.R(1);
        sp.previous();
        sp.remove();
        sp.add(new StgEnter(ret));
        tso.whatNext = ThreadComplete;
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }

    @Override
    public boolean doRaiseAsync(Capability cap, TSO tso, Closure exception, boolean stopAtAtomically, Thunk updatee, AtomicReference<Closure> topClosure) {
        tso.whatNext = ThreadKilled;
        tso.sp.remove();
        return false;
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, TSO tso, AtomicReference<Closure> raiseClosure, Closure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, TSO tso, Closure exception) {
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
    public MarkFrameResult mark(Capability cap, TSO tso) { return Stop; }
}
