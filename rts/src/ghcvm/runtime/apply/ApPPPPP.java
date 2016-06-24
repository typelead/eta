package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class ApPPPPP extends StackFrame {
    private final StgClosure p1;
    private final StgClosure p2;
    private final StgClosure p3;
    private final StgClosure p4;
    private final StgClosure p5;

    public ApPPPPP(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4, StgClosure p5) {
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p1, p2, p3, p4, p5);
    }
}
