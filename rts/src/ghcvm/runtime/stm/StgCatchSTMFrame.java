package ghcvm.runtime.stm;

import java.util.ListIterator;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class StgCatchSTMFrame extends StgSTMCatchFrame {
    public final StgClosure code;
    public final StgClosure handler;

    public StgCatchSTMFrame(final StgClosure code, StgClosure handler) {
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
            context.R1 = code;
            Apply.ap_v_fast.enter(context);
        }
    }

}
