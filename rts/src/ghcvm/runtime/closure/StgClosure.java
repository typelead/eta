package ghcvm.runtime.closure;

public abstract class StgClosure {
    public abstract void enter(StgContext context);
}
