package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPPPP extends StackFrame {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public StgClosure p4;

    public ApPPPP(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4) {
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p1, p2, p3, p4);
    }
}
