package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApN extends StackFrame {
    public int n;

    public ApN(int n) {
        this.n = n;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, n);
    }
}
