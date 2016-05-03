package ghcvm.runtime.closure;

public class StgIndStatic {
    StgClosure indirectee;

    public StgIndStatic(StgClosure indirectee) {
        this.indirectee = indirectee;
    }

    @Override
    public void enter(StgContext context) {
        indirectee.enter(context);
    }
}
