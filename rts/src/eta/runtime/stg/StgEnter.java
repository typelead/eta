package eta.runtime.stg;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class StgEnter extends StackFrame {
    public final Closure closure;

    public StgEnter(final Closure closure) {
        this.closure = closure;
    }

    @Override
    public void stackEnter(StgContext context) {
        closure.evaluate(context);
    }

    @Override
    public Closure getClosure() {
        return closure;
    }
}
