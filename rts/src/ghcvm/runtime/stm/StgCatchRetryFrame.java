package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.apply.Apply;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class StgCatchRetryFrame extends StackFrame {
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
        Stack<StgTRecHeader> stack = tso.trec;
        ListIterator<StgTRecHeader> it = stack.listIterator(stack.size());
        StgTRecHeader trec = it.previous();
        StgTRecHeader outer = it.previous();
        boolean result = cap.stmCommitNestedTransaction(trec, outer);
        if (result) {
            StgTRecHeader newTrec = cap.stmStartTransaction(outer);
            stack.pop();
            stack.push(newTrec);
            if (runningAltCode) {
                context.R1 = altCode;
            } else {
                context.R1 = firstCode;
            }
            tso.sp.add(new StgCatchRetryFrame(firstCode, altCode, runningAltCode));
            Apply.ap_v_fast.enter(context);
        } else {
            tso.trec.pop();
        }
    }

}
