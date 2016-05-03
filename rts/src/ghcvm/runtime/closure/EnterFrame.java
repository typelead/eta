package ghcvm.runtime.closure;

public class EnterFrame extends StackFrame {
    StgClosure closure;

    public EnterFrame(StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        closure.enter(context);
    }
}
