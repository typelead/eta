package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

import ghcvm.runtime.exception.StgException;
import ghcvm.runtime.exception.StgReturnException;

public class ReturnToSchedulerButFirst extends StgClosure {

    @Override
    public void enter(StgContext context) {
        context.myCapability.threadPaused(context.currentTSO);
        context.R2.enter(context);
    }
}
