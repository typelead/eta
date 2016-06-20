package ghcvm.runtime.stm;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.thunk.StgInd;

public abstract class StgSTMCatchFrame extends StackFrame {
    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        /* TODO: Implement after refactor of STM */
        return true;
    }

}
