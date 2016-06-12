package ghcvm.runtime.stackframe;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class ReturnClosure extends StackFrame {
    public final StgClosure closure;

    public ReturnClosure(final StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R1 = closure;
    }
}
