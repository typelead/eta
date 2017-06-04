package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPPV extends StackFrame {
    public StgClosure p1;
    public StgClosure p2;

    public ApPPV(final StgClosure p1, final StgClosure p2) {
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1).applyPPV(context, p1, p2);
    }
}
