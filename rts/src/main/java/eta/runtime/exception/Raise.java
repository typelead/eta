package eta.runtime.exception;

import eta.exception.Exception;
import eta.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.SingleEntryThunk;

public class Raise extends SingleEntryThunk {
    public final Closure exception;

    public Raise(final Closure exception) {
        this.exception = exception;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return Exception.raise(context, exception);
    }
}
