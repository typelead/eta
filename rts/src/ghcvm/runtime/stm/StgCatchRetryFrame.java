package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.apply.Apply;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.StackFrame;

public class StgCatchRetryFrame extends StackFrame {
    public final boolean runningAltCode;
    public final StgClosure firstCode;
    public final StgClosure altCode;

    public StgCatchRetryFrame(final StgClosure firstCode, final StgClosure altCode, final boolean runningAltCode) {
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
                Apply.ap_v_fast.enter(context);
                // TODO: jump with stack
            } else {
                context.R1 = firstCode;
                Apply.ap_v_fast.enter(context);
                // TODO: jump with stack
            }
        } else {
            tso.trec.pop();
        }
    }

}
