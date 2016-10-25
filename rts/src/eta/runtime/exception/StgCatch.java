package eta.runtime.exception;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.StgFun;

public class StgCatch extends StgFun {
    /* TODO: Extend from StgFun instead? */
    public final StgClosure io;
    public final StgClosure handler;

    public StgCatch(final StgClosure io, final StgClosure handler) {
        this.io = io;
        this.handler = handler;
    }

    /* TODO: Is it the correct arity? */
    @Override
    public int getArity() { return 0; }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R(1, io);
        context.R(2, handler);
        StgException.catch_.enter(context);
    }
}
