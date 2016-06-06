package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

import ghcvm.runtime.exception.StgException;
import ghcvm.runtime.exception.StgReturnException;

public class ReturnToScheduler extends StgClosure {

    @Override
    public void enter(StgContext context) {
        context.myCapability.threadPaused(context.currentTSO);
        throw StgException.stgReturnException;
    }
}
