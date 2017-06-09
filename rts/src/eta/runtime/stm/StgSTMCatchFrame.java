package eta.runtime.stm;

import java.util.concurrent.atomic.AtomicReference;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StackFrame;
import eta.runtime.thunk.Thunk;

public abstract class StgSTMCatchFrame extends StgSTMFrame {
    @Override
    public boolean doRaiseAsync(Capability cap, TSO tso, Closure exception, boolean stopAtAtomically, Thunk updatee, AtomicReference<Closure> topClosure) {
        TransactionRecord trec = tso.trec;
        TransactionRecord outer = trec.enclosingTrec;
        cap.stmAbortTransaction(trec);
        cap.stmFreeAbortedTrec(trec);
        tso.trec = outer;
        return true;
    }

}
