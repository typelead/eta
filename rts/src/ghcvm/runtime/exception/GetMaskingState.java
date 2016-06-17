package ghcvm.runtime.exception;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadFinished;
import static ghcvm.runtime.types.StgTSO.*;

public class GetMaskingState extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgTSO tso = context.currentTSO;
        context.I1 = ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                      (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0));
    }
}
