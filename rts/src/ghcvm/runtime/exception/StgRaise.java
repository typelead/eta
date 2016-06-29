package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgThunk;

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
