package eta.runtime.exception;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import static eta.runtime.stg.StgTSO.TSO_BLOCKEX;
import static eta.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;

public class MaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO tso = context.currentTSO;
        tso.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
}
