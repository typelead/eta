package eta.runtime.exception;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;

public class MaskUninterruptibleFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO tso = context.currentTSO;
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
    }
}
