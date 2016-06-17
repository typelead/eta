package ghcvm.runtime.exception;

import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.closure.StgContext;
import static ghcvm.runtime.types.StgTSO.*;

public class MaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        context.currentTSO.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
}
