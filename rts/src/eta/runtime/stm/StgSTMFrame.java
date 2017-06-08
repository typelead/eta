package eta.runtime.stm;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StackFrame;

public abstract class StgSTMFrame extends StackFrame {

    public boolean doRetry(Capability cap, TSO tso, StgTRecHeader trec) {
        /* TODO: Throw an exception here */
        return false;
    }
}
