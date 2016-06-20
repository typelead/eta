package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class StgCatch extends StgClosure {
    /* TODO: Extend from StgFun instead? */
    public final StgClosure io;
    public final StgClosure handler;

    public StgCatch(final StgClosure io, final StgClosure handler) {
        this.io = io;
        this.handler = handler;
    }

    @Override
    public void enter(StgContext context) {
        context.R1 = io;
        context.R2 = handler;
        StgException.catch_.enter(context);
    }
}
