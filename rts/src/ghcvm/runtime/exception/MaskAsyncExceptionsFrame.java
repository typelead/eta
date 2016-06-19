package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StgTSO.*;

public class MaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        context.currentTSO.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
}
