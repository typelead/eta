package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class ApP extends StackFrame {
    public StgClosure p;

    public ApP(StgClosure p) {
        this.p = p;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p);
    }
}
