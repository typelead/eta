package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class StgRaise extends StgClosure {
    /* TODO: Extend from StgInd or StgThunk instead? */
    public final StgClosure exception;

    public StgRaise(final StgClosure exception) {
        this.exception = exception;
    }

    @Override
    public void enter(StgContext context) {
        context.R1 = exception;
        StgException.raise.enter(context);
    }
}
