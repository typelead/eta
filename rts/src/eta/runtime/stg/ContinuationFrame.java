package eta.runtime.stg;

public class ContinuationFrame extends StackFrame {

    public final StgClosure closure;
    public final int target;
    public final ArgumentStack argStack;
    public final ArgumentStack localsStack;
    public final ArgumentStack returnStack;

    public ContinuationFrame(StgClosure closure, int target, ArgumentStack argStack,
                             ArgumentStack localsStack, ArgumentStack returnStack)
    {
        this.closure = closure;
        this.target = target;
        this.argStack = argStack;
        this.localsStack = localsStack;
        this.returnStack = returnStack;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.target = target;
        context.argStack = argStack;
        context.localsStack = localsStack;
        context.returnStack = returnStack;
        closure.enter(context);
    }
}
