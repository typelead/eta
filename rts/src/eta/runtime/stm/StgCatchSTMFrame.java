package eta.runtime.stm;

import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;


public class StgCatchSTMFrame extends StgSTMCatchFrame {
    public final Closure code;
    public final Closure handler;

    public StgCatchSTMFrame(final Closure code, Closure handler) {
        this.code = code;
        this.handler = handler;
    }

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        ListIterator<StackFrame> sp = tso.sp;
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        boolean result = cap.stmCommitNestedTransaction(trec);;
        if (result) {
            tso.trec = outer;
        } else {
            StgTRecHeader newTrec = cap.stmStartTransaction(outer);
            tso.trec = newTrec;
            sp.add(new StgCatchSTMFrame(code, handler));
            code.applyV(context);
        }
    }

    @Override
    public boolean doFindRetry(Capability cap, StgTSO tso) {
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        tso.trec = outer;
        return true;
    }

    @Override
    public boolean doRaiseExceptionHelper(Capability cap, StgTSO tso, AtomicReference<Closure> raiseClosure, Closure exception) {
        tso.sp.next();
        return false;
    }

    @Override
    public boolean doRaise(StgContext context, Capability cap, StgTSO tso, Closure exception) {
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        tso.trec = outer;
        tso.spPop();
        handler.applyPV(context, exception);
        return false;
    }
}
