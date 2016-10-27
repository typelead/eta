package eta.runtime.stg;

public class ReturnClosure extends StackFrame {
    public final StgClosure closure;

    public ReturnClosure(final StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1, closure);
    }
}
