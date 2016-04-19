package ghcvm.runtime.closure;

public class EnterFrame extends StackFrame {
    StgClosure closure;
    public EnterFrame(StgClosure closure) {
        this.closure = closure;
    }

    public void enter(StgContext context) {
        super.enter(context);
        closure.enter(context);
    }
}
