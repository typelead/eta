package ghcvm.runtime.exception;

import java.util.ListIterator;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgInd;
import static ghcvm.runtime.stg.StgTSO.TSO_BLOCKEX;
import static ghcvm.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgCatchFrame extends StackFrame {
    public final int exceptionsBlocked;
    public final StgClosure handler;

    public StgCatchFrame(int exceptionsBlocked, final StgClosure handler) {
        this.exceptionsBlocked = exceptionsBlocked;
        this.handler = handler;
    }

    @Override
    public void stackEnter(StgContext context) {
        /* This frame just sets context.R1 = context.R1,
           a trivial operation. Hence, the body is empty. */
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        ListIterator<StackFrame> sp = tso.sp;
        if (exception == null) {
            sp.previous();
            return true;
        } else {
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
            StgClosure raise = new StgRaise(exception);
            sp.add(new StgEnter(raise));
            tso.whatNext = ThreadRunGHC;
            return false;
        }
    }

}
