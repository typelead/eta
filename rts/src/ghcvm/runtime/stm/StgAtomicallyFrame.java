package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.ReturnClosure;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.apply.Apply;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgAtomicallyFrame extends StgAtomicallyBaseFrame {

    public StgAtomicallyFrame(final StgClosure code) {
        this(code, new ArrayDeque<StgInvariantCheck>(), null);
    }

    public StgAtomicallyFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result) {
        super(code, invariants, result);
    }

    @Override
    public void stackEnter(StgContext context) {
        /* TODO: Logic is VERY iffy here. Recheck to make sure it's as
                 intended. */
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        Queue<StgInvariantCheck> invariants = null;
        if (outer == null) {
            invariants = cap.stmGetInvariantsToCheck(trec);
            result = context.R1;
        } else {
            tso.trec = outer;
            invariants = this.invariants;
            StgInvariantCheck check = invariants.peek();
            check.myExecution = trec;
            cap.stmAbortTransaction(trec);
            invariants.poll();
            trec = outer;
        }

        if (invariants.isEmpty()) {
            boolean valid = cap.stmCommitTransaction(trec);
            if (valid) {
                tso.trec = null;
                context.R1 = result;
            } else {
                StgTRecHeader newTrec = cap.stmStartTransaction(null);
                tso.trec = newTrec;
                sp.add(new StgAtomicallyFrame(code, invariants, result));
                context.R1 = code;
                Apply.ap_v_fast.enter(context);
            }
        } else {
            trec = cap.stmStartTransaction(trec);
            tso.trec = trec;
            StgInvariantCheck q = invariants.peek();
            StgAtomicInvariant invariant = q.invariant;
            context.R1 = invariant.code;
            /* TODO: Ensure that creating a new is the right thing */
            sp.add(new StgAtomicallyFrame(code, invariants, result));
            Apply.ap_v_fast.enter(context);
        }
    }
}
