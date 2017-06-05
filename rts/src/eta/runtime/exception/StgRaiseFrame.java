package eta.runtime.exception;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class StgRaiseFrame extends StackFrame {
    public final StgClosure exception;

    public StgRaiseFrame(final StgClosure exception) {
        this.exception = exception;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgException.noBreakOnException = true;
        StgException.raise(context, exception);
    }
}
