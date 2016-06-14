package ghcvm.runtime.stackframe;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StgEnter extends StackFrame {
    public final StgClosure closure;

    public StgEnter(final StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        closure.enter(context);
    }
}
