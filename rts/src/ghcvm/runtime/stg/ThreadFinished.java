package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.closure.StgContext.ReturnCode.ThreadFinished;

public class ThreadFinished extends StgClosure {

    @Override
    public void enter(StgContext context) {
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }
}
