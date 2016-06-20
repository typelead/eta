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

public class StgAtomicallyFrame extends StackFrame {
    public final StgClosure code;
    public Queue<StgInvariantCheck> nextInvariants = new ArrayDeque<StgInvariantCheck>();
    public StgClosure result;

    public StgAtomicallyFrame(final StgClosure code) {
        this.code = code;
    }

    public StgAtomicallyFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result) {
        this.code = code;
        nextInvariants = invariants;
        this.result = result;
    }

    @Override
    public void stackEnter(StgContext context) {
        // TODO: Complete
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        Stack<StgTRecHeader> stack = tso.trec;
        ListIterator<StgTRecHeader> it = stack.listIterator(stack.size());
        StgTRecHeader trec = it.previous();
        StgTRecHeader outer = null;
        StgClosure result = this.result;
        if (it.hasPrevious()) {
            outer = it.previous();
        }
        Queue<StgInvariantCheck> invariants = null;
        if (outer == null) {
            invariants = cap.stmGetInvariantsToCheck(trec);
            result = context.R1;
        } else {
            invariants = nextInvariants;
            StgInvariantCheck check = nextInvariants.peek();
            check.myExecution = trec; // TODO: Should this be trec stack?
            cap.stmAbortTransaction(stack);
            nextInvariants.poll();
            trec = outer;
            stack.pop();
        }

        if (invariants.isEmpty()) {
            boolean valid = cap.stmCommitTransaction(stack);
            if (valid) {
            } else {
            }
        } else {
            trec = cap.stmStartTransaction(trec);
            stack.push(trec);
            StgInvariantCheck q = invariants.peek();
            StgAtomicInvariant invariant = q.invariant;
            context.R1 = invariant.code;
            sp.add(new StgAtomicallyFrame(code, invariants, result));
            Apply.ap_v_fast.enter(context);
        }
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        ListIterator<StackFrame> sp = tso.sp;
        if (stopAtAtomically) {
            cap.stmCondemnTransaction(tso.trec);
            /* TODO: Should a separate value be used
                     instead of null? */
            sp.add(new ReturnClosure(null));
            tso.whatNext = ThreadRunGHC;
            return false;
        } else {
            /* TODO: Implement after refactor of STM */
            sp.previous();
            return true;
        }
    }
}
