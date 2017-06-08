package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class StgRaiseFrame extends StackFrame {
    public final Closure exception;

    public StgRaiseFrame(final Closure exception) {
        this.exception = exception;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgException.noBreakOnException = true;
        StgException.raise(context, exception);
    }
}
