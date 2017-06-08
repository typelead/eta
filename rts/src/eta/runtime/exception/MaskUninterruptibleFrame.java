package eta.runtime.exception;

import eta.runtime.stg.TSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import static eta.runtime.stg.TSO.TSO_BLOCKEX;
import static eta.runtime.stg.TSO.TSO_INTERRUPTIBLE;

public class MaskUninterruptibleFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        TSO tso = context.currentTSO;
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
    }
}
