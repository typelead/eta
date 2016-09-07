package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import ghcvm.runtime.stg.Stg;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.stg.ReturnClosure;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgThunk;
import ghcvm.runtime.apply.Apply;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;

public class StgAtomicallyFrame extends StgSTMFrame {
    public final StgClosure code;
    public Queue<StgInvariantCheck> invariants = new ArrayDeque<StgInvariantCheck>();
    public StgClosure result;
    public boolean waiting;

    public StgAtomicallyFrame(final StgClosure code) {
        this(code, new ArrayDeque<StgInvariantCheck>(), null);
    }

    public StgAtomicallyFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result) {
        this(code, invariants, result, false);
    }

    public StgAtomicallyFrame(final StgClosure code, Queue<StgInvariantCheck> invariants, StgClosure result, boolean waiting) {
        this.code = code;
        this.invariants = invariants;
        this.result = result;
        this.waiting = waiting;
    }

    @Override
    public void stackEnter(StgContext context) {
        /* TODO: Logic is VERY iffy here. Recheck to make sure it's as
                 intended. */
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        StgTRecHeader trec = tso.trec;
        if (waiting) {
            boolean valid = cap.stmReWait(tso);
            if (valid) {
                sp.add(new StgAtomicallyFrame(code, invariants, result, true));
                Stg.block_noregs.enter(context);
            } else {
                StgTRecHeader newTrec = cap.stmStartTransaction(null);
                tso.trec = newTrec;
                sp.add(new StgAtomicallyFrame(code, invariants, result));
                context.R(1, code);
                Apply.ap_v_fast.enter(context);
            }
        } else {
            StgTRecHeader outer = trec.enclosingTrec;
            Queue<StgInvariantCheck> invariants = null;
            if (outer == null) {
                invariants = cap.stmGetInvariantsToCheck(trec);
                result = context.R(1);
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
                    context.R(1, result);
                } else {
                    StgTRecHeader newTrec = cap.stmStartTransaction(null);
                    tso.trec = newTrec;
                    sp.add(new StgAtomicallyFrame(code, invariants, result));
                    context.R(1, code);
                    Apply.ap_v_fast.enter(context);
                }
            } else {
                trec = cap.stmStartTransaction(trec);
                tso.trec = trec;
                StgInvariantCheck q = invariants.peek();
                StgAtomicInvariant invariant = q.invariant;
                context.R(1, invariant.code);
                /* TODO: Ensure that creating a new is the right thing */
                sp.add(new StgAtomicallyFrame(code, invariants, result));
                Apply.ap_v_fast.enter(context);
            }
        }
    }

    @Override
    public boolean doFindRetry(Capability cap, StgTSO tso) {
        return false;
    }

    @Override
    public boolean doRetry(Capability cap, StgTSO tso, StgTRecHeader trec) {
        /* TODO: Verify that adjusting the context like this is valid. */
        StgContext context = cap.context;
        StgTRecHeader outer = trec.enclosingTrec;
        if (outer != null) {
            cap.stmAbortTransaction(trec);
            cap.stmFreeAbortedTrec(trec);
            trec = outer;
            tso.trec = trec;
            outer = trec.enclosingTrec;
        }
        boolean result = cap.stmWait(tso, trec);
        if (result) {
            waiting = true;
            /* TODO: Adjust stack top to be this frame. */
            context.R(3, trec);
            STM.block_stmwait.enter(context);
            return false;
        } else {
            StgTRecHeader newTrec = cap.stmStartTransaction(outer);
            tso.trec = newTrec;
            /* TODO: Adjust stack top to be this frame */
            context.R(1, code);
            Apply.ap_v_fast.enter(context);
            return false;
        }
    }

    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        ListIterator<StackFrame> sp = tso.sp;
        if (stopAtAtomically) {
            cap.stmCondemnTransaction(tso.trec);
            sp.next(); //Point after Atomically frame
            // Remove all frames afterward
            while (sp.hasNext()) {
                sp.next();
                sp.remove();
            }
            sp.add(new ReturnClosure(null));
            tso.whatNext = ThreadRunGHC;
            return false;
        } else {
            StgTRecHeader trec = tso.trec;
            StgTRecHeader outer = trec.enclosingTrec;
            cap.stmAbortTransaction(trec);
            cap.stmFreeAbortedTrec(trec);
            tso.trec = outer;
            // Remove all frames including this one
            while (sp.hasNext()) {
                sp.next();
                sp.remove();
            }
            StgClosure atomically = new StgAtomically(code);
            topClosure.set(atomically);
            return true;
        }
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<StgClosure> raiseClosure, StgClosure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, StgTSO tso, StgClosure exception) {
        StgTRecHeader trec = tso.trec;
        boolean result = cap.stmValidateNestOfTransactions(trec);
        StgTRecHeader outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        if (outer != null) {
            cap.stmAbortTransaction(outer);
            cap.stmFreeAbortedTrec(outer);
        }
        tso.trec = null;
        if (result) {
            tso.spPop();
            return true;
        } else {
            trec = cap.stmStartTransaction(null);
            tso.trec = trec;
            context.R(1, code);
            Apply.ap_v_fast.enter(context);
            return false;
        }
    }
}
