package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Thunk;

public class Raise extends Thunk {
    /* TODO: Should this be an UpdatableThunk? */
    public final Closure exception;

    public Raise(final Closure exception) {
        this.exception = exception;
    }

    @Override
    public Closure enter(StgContext context) {
        return Exception.raise(context, exception);
    }
}
