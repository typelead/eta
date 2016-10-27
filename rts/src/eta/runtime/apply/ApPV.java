package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPV extends StackFrame {
    public StgClosure p;

    public ApPV(StgClosure p) {
        this.p = p;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p, Void._void);
    }
}
