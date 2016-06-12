package ghcvm.runtime.stg;

import ghcvm.runtime.Stg;
import ghcvm.runtime.exception.StgException;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class BlockReadMVarFinally extends StgClosure {

    @Override
    public void enter(StgContext context) {
        throw StgException.stgReturnException;
    }
}
