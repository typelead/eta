package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.Function;

public class StgCatch extends Function {
    /* TODO: Extend from Function instead? */
    public final Closure io;
    public final Closure handler;

    public StgCatch(final Closure io, final Closure handler) {
        this.io = io;
        this.handler = handler;
    }

    /* TODO: Is it the correct arity? */
    @Override
    public int getArity() { return 0; }

    @Override
    public Closure enter(StgContext context) {
        context.R(1, this); // TODO: Verify
        return StgException.catch_(context, io, handler);
    }
}
