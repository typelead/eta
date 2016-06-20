package ghcvm.runtime.stm;

import ghcvm.runtime.stg.StackFrame;

public abstract class StgSTMCatchFrame extends StackFrame {
    @Override
    public boolean doRaiseAsync(Capability cap, StgTSO tso, StgClosure exception, boolean stopAtAtomically, StgInd updatee) {
        /* TODO: Implement after refactor of STM */
        return true;
    }

}
