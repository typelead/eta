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
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.apply.Apply;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public abstract class StgAtomicallyBaseFrame extends StgSTMFrame {
    public final StgClosure code;
    public Queue<StgInvariantCheck> invariants = new ArrayDeque<StgInvariantCheck>();
    public StgClosure result;

    public StgAtomicallyBaseFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result) {
        this.code = code;
        this.invariants = invariants;
        this.result = result;
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
