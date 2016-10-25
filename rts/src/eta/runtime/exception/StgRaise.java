package eta.runtime.exception;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.StgThunk;

public class StgRaise extends StgThunk {
    /* TODO: Should this be an StgInd? */
    public final StgClosure exception;

    public StgRaise(final StgClosure exception) {
        this.exception = exception;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R(1, exception);
        StgException.raise.enter(context);
    }
}
