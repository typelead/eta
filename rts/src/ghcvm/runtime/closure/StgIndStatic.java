package ghcvm.runtime.closure;

public class StgIndStatic extends StgClosure {
    StgClosure indirectee;

    public StgIndStatic(StgClosure indirectee) {
        this.indirectee = indirectee;
    }

    @Override
    public void enter(StgContext context) {
        indirectee.enter(context);
    }
}
