package ghcvm.runtime.closure;

public class StgEnter extends StackFrame {
    StgClosure closure;

    public StgEnter(StgClosure closure) {
        this.closure = closure;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        closure.enter(context);
    }
}
