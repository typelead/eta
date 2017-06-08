package eta.runtime.stm;

import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StackFrame;
import eta.runtime.thunk.StgThunk;

public abstract class StgSTMCatchFrame extends StgSTMFrame {
    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, Closure exception, boolean stopAtAtomically, StgThunk updatee, AtomicReference<Closure> topClosure) {
        StgTRecHeader trec = tso.trec;
        StgTRecHeader outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        tso.trec = outer;
        return true;
    }

}
