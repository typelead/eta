package eta.runtime.stg;

public class ReturnClosure extends StackFrame {
    public final Closure closure;

    public ReturnClosure(final Closure closure) {
        this.closure = closure;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1, closure);
    }
}
