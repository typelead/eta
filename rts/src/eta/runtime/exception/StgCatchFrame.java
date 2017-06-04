package eta.runtime.exception;

import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.StgThunk;
import eta.runtime.apply.Apply;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgCatchFrame extends StackFrame {
    public final int exceptionsBlocked;
    public final StgClosure handler;

    public StgCatchFrame(int exceptionsBlocked, final StgClosure handler) {
        this.exceptionsBlocked = exceptionsBlocked;
        this.handler = handler;
    }

    @Override
    public void stackEnter(StgContext context) {}
    /* This frame just sets context.R(1) to itself,
       a trivial operation. Hence, the body is empty. */

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        ListIterator<StackFrame> sp = tso.sp;
        if (exception == null) {
            return true;
        } else {
            sp.next(); //Point after Catch frame
            // Remove all frames afterward
            while (sp.hasNext()) {
                sp.next();
                sp.remove();
            }
            tso.addFlags(TSO_BLOCKEX);
            if ((exceptionsBlocked & TSO_INTERRUPTIBLE) == 0) {
                tso.removeFlags(TSO_INTERRUPTIBLE);
            } else {
                tso.addFlags(TSO_INTERRUPTIBLE);
            }
            StgRaise raise = new StgRaise(exception);
            tso.spPush(new StgEnter(raise));
            tso.whatNext = ThreadRunGHC;
            return false;
        }
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, StgTSO tso, StgClosure exception) {
        tso.spPop();
        if ((exceptionsBlocked & TSO_BLOCKEX) == 0) {
            tso.spPush(new UnmaskAsyncExceptionsFrame());
        }
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
        if ((exceptionsBlocked & (TSO_BLOCKEX | TSO_INTERRUPTIBLE)) == TSO_BLOCKEX) {
            tso.removeFlags(TSO_INTERRUPTIBLE);
        }
        context.R(1, handler);
        context.R(2, exception);
        handler.applyPV(context, exception);
        return false;
    }
}
