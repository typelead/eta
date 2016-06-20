package ghcvm.runtime.stm;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;

public abstract class StgSTMFrame extends StackFrame {

    public boolean doRetry(Capability cap, StgTSO tso, StgTRecHeader trec) {
        /* TODO: Throw an exception here */
        return false;
    }
}
