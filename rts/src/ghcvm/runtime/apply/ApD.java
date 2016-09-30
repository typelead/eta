package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class ApD extends StackFrame {
    public double d;

    public ApD(double d) {
        this.d = d;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, d);
    }
}
