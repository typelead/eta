package eta.runtime.stg;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public class StgEnter extends StackFrame {
    public final StgClosure closure;

    public StgEnter(final StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void stackEnter(StgContext context) {
        closure.evaluate(context);
    }

    @Override
    public StgClosure getClosure() {
        return closure;
    }
}
