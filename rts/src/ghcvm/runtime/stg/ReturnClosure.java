package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class ReturnClosure extends StackFrame {
    public final StgClosure closure;

    public ReturnClosure(final StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R1 = closure;
    }
}
