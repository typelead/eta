package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class StgRaiseFrame extends StackFrame {
    public final StgClosure exception;

    public StgRaiseFrame(final StgClosure exception) {
        this.exception = exception;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgException.noBreakOnException = true;
        context.R1 = exception;
        StgException.raise.enter(context);
    }
}
