package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.ReturnClosure;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.apply.Apply;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgAtomicallyWaitingFrame extends StgAtomicallyBaseFrame {

    public StgAtomicallyWaitingFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result) {
        super(code, invariants, result);
    }

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        boolean valid = cap.stmReWait(tso);
        if (valid) {
            sp.add(new StgAtomicallyWaitingFrame(code, invariants, result));
            Stg.block_noregs.enter(context);
        } else {
            StgTRecHeader trec = cap.stmStartTransaction(null);
            tso.trec = trec;
            sp.add(new StgAtomicallyFrame(code, invariants, result));
            context.R1 = code;
            Apply.ap_v_fast.enter(context);
        }
    }

}
