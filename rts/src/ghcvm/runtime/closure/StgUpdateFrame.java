package ghcvm.runtime.closure;

public class StgUpdateFrame extends StackFrame {
    StgInd updatee;

    public StgUpdateFrame(StgInd updatee) {
        this.updatee = updatee;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgClosure ret = context.R1;
        // TODO: overwriting_closure only if profiling OR (! threaded && debug)
        updatee.indirectee = ret;
        // TODO: if threaded, write barrier;
        updatee.blackhole = true;
        context.R1 = ret;
    }
}
