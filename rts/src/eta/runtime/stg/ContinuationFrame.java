package eta.runtime.stg;

public class ContinuationFrame extends StackFrame {

    public final StgClosure closure;
    public final int target;
    public final ArgumentStack argStack;
    public final ArgumentStack returnStack;

    public ContinuationFrame(StgClosure closure, int target,
                             ArgumentStack returnStack, ArgumentStack argStack)
    {
        this.closure = closure;
        this.target = target;
        this.returnStack = returnStack;
        this.argStack = argStack;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.target = target;
        context.argStack = argStack;
        context.returnStack = returnStack;
        closure.enter(context);
    }
}
