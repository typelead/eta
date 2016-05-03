package ghcvm.runtime.closure;

public class StackFrame extends StgClosure {
    public StackFrame prev;
    public StackFrame next;

    public void enter(StgContext context) {
        while (next != null) {
            next.enter(context);
        }
    }
}
