package ghcvm.runtime.exception;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadFinished;
import static ghcvm.runtime.types.StgTSO.*;

public class MaskAsyncExceptionsFrame extends StackFrame {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.currentTSO.addFlags(TSO_BLOCKEX | TSO_INTERRUPTIBLE);
    }
}
