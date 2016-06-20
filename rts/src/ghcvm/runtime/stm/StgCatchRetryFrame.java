package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.apply.Apply;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class StgCatchRetryFrame extends StgSTMCatchFrame {
    public boolean runningAltCode;
    public final StgClosure firstCode;
    public final StgClosure altCode;

    public StgCatchRetryFrame(final StgClosure firstCode, final StgClosure altCode) {
        this(firstCode, altCode, false);
    }

    public StgCatchRetryFrame(final StgClosure firstCode, final StgClosure altCode, boolean runningAltCode) {
        this.firstCode = firstCode;
        this.altCode = altCode;
        this.runningAltCode = runningAltCode;
    }

    @Override
    public void stackEnter(StgContext context) {
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        boolean result = cap.stmCommitNestedTransaction(trec);
        if (result) {
            tso.trec = outer;
        } else {
            StgTRecHeader newTrec = cap.stmStartTransaction(outer);
            tso.trec = newTrec;
            if (runningAltCode) {
                context.R1 = altCode;
            } else {
                context.R1 = firstCode;
            }
            tso.sp.add(new StgCatchRetryFrame(firstCode, altCode, runningAltCode));
            Apply.ap_v_fast.enter(context);
        }
    }

}
