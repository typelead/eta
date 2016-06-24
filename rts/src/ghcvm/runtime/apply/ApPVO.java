package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StackFrame;

public class ApPVO extends StackFrame {
    private final StgClosure p;
    private final Object o;

    public ApPVO(StgClosure p, Object o) {
        this.p = p;
        this.o = o;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p, null, o);
    }
}
