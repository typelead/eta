package ghcvm.runtime.stm;

import java.util.concurrent.atomic.AtomicReference;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.thunk.StgThunk;

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
