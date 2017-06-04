package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPPPPPP extends StackFrame {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public StgClosure p4;
    public StgClosure p5;
    public StgClosure p6;

    public ApPPPPPP(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4, StgClosure p5, StgClosure p6) {
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
        this.p6 = p6;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1).applyPPPPPP(context, p1, p2, p3, p4, p5, p6);
    }
}
