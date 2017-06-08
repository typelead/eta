package eta.runtime.stg;

public class ContinuationFrame extends StackFrame {

    public final Closure closure;
    public final int target;
    public final ArgumentStack argStack;
    public final ArgumentStack localsStack;

    public ContinuationFrame(Closure closure, int target, ArgumentStack argStack,
                             ArgumentStack localsStack) {
        this.closure = closure;
        this.target = target;
        this.argStack = argStack;
        this.localsStack = localsStack;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.target = target;
        context.argStack = argStack;
        context.localsStack = localsStack;
        closure.enter(context);
    }
}
