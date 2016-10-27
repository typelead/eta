package eta.runtime.stm;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;

public abstract class StgSTMFrame extends StackFrame {

    public boolean doRetry(Capability cap, StgTSO tso, StgTRecHeader trec) {
        /* TODO: Throw an exception here */
        return false;
    }
}
