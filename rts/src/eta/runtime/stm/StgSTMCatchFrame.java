package eta.runtime.stm;

import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StackFrame;
import eta.runtime.thunk.StgThunk;

public abstract class StgSTMCatchFrame extends StgSTMFrame {
    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<StgClosure> topClosure) {
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        tso.trec = outer;
        return true;
    }

}
