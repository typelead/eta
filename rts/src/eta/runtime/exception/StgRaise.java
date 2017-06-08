package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Thunk;

public class StgRaise extends Thunk {
    /* TODO: Should this be an StgInd? */
    public final Closure exception;

    public StgRaise(final Closure exception) {
        this.exception = exception;
    }

    @Override
    public Closure enter(StgContext context) {
        context.R(1, this); // TODO: Verify
        return StgException.raise(context, exception);
    }
}
