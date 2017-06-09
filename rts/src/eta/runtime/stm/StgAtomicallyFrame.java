package eta.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.stg.ReturnClosure;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Thunk;

import static eta.runtime.RtsMessages.barf;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;

public class StgAtomicallyFrame extends StgSTMFrame {
    public final Closure code;
    public Queue<InvariantCheck> invariants = new ArrayDeque<InvariantCheck>();
    public Closure result;
    public boolean waiting;

    public StgAtomicallyFrame(final Closure code) {
        this(code, new ArrayDeque<InvariantCheck>(), null);
    }

    public StgAtomicallyFrame(final Closure code, Queue<InvariantCheck> invariants, Closure result) {
        this(code, invariants, result, false);
    }

    public StgAtomicallyFrame(final Closure code, Queue<InvariantCheck> invariants, Closure result, boolean waiting) {
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
        TSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        TransactionRecord trec = tso.trec;
        if (waiting) {
            boolean valid = cap.stmReWait(tso);
            if (valid) {
                sp.add(new StgAtomicallyFrame(code, invariants, result, true));
                // Stg.block_noregs.enter(context);
                barf("StgAtomicallyFrame: unimplemented RTS primop");
            } else {
                TransactionRecord newTrec = cap.stmStartTransaction(null);
                tso.trec = newTrec;
                sp.add(new StgAtomicallyFrame(code, invariants, result));
                code.applyV(context);
            }
        } else {
            TransactionRecord outer = trec.enclosingTrec;
            Queue<InvariantCheck> invariants = null;
            if (outer == null) {
                invariants = cap.stmGetInvariantsToCheck(trec);
                result = context.R(1);
            } else {
                tso.trec = outer;
                invariants = this.invariants;
                InvariantCheck check = invariants.peek();
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
                    TransactionRecord newTrec = cap.stmStartTransaction(null);
                    tso.trec = newTrec;
                    sp.add(new StgAtomicallyFrame(code, invariants, result));
                    code.applyV(context);
                }
            } else {
                trec = cap.stmStartTransaction(trec);
                tso.trec = trec;
                InvariantCheck q = invariants.peek();
                AtomicInvariant invariant = q.invariant;
                /* TODO: Ensure that creating a new is the right thing */
                sp.add(new StgAtomicallyFrame(code, invariants, result));
                invariant.code.applyV(context);
            }
        }
    }

    @Override
    public boolean doFindRetry(Capability cap, TSO tso) {
        return false;
    }

    @Override
    public boolean doRetry(Capability cap, TSO tso, TransactionRecord trec) {
        /* TODO: Verify that adjusting the context like this is valid. */
        StgContext context = cap.context;
        TransactionRecord outer = trec.enclosingTrec;
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
            // STM.block_stmwait.enter(context);
            barf("retry#: unimplemented RTS primop.");
            return false;
        } else {
            TransactionRecord newTrec = cap.stmStartTransaction(outer);
            tso.trec = newTrec;
            /* TODO: Adjust stack top to be this frame */
            code.applyV(context);
            return false;
        }
    }

    @Override
    public boolean doRaiseAsync(Capability cap, TSO tso, Closure exception, boolean stopAtAtomically, Thunk updatee, AtomicReference<Closure> topClosure) {
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
            tso.whatNext = ThreadRun;
            return false;
        } else {
            TransactionRecord trec = tso.trec;
            TransactionRecord outer = trec.enclosingTrec;
            cap.stmAbortTransaction(trec);
            cap.stmFreeAbortedTrec(trec);
            tso.trec = outer;
            // Remove all frames including this one
            while (sp.hasNext()) {
                sp.next();
                sp.remove();
            }
            Closure atomically = new StgAtomically(code);
            topClosure.set(atomically);
            return true;
        }
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, TSO tso, AtomicReference<Closure> raiseClosure, Closure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, TSO tso, Closure exception) {
        TransactionRecord trec = tso.trec;
        boolean result = cap.stmValidateNestOfTransactions(trec);
        TransactionRecord outer = trec.enclosingTrec;
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
            code.applyV(context);
            return false;
        }
    }
}
