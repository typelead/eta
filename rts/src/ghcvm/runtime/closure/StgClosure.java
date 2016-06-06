package ghcvm.runtime.closure;

public abstract class StgClosure {
    public void enter(StgContext context) {}
    public void preEnter(StgContext context) {}
}
