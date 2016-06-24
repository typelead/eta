package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class ApPP extends StackFrame {
    private final StgClosure p1;
    private final StgClosure p2;

    public ApPP(final StgClosure p1, final StgClosure p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p1, p2);
    }
}
