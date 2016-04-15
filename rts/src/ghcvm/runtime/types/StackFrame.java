package ghcvm.runtime.types;

public abstract class StackFrame implements StgClosure {
    public StackFrame prev;
    public abstract void enter();
}
