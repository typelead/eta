package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPPPVO extends StackFrame {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public Object o;

    public ApPPPVO(final StgClosure p1, final StgClosure p2, final StgClosure p3, final Object o) {
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.o = o;
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure fun = context.R(1);
        fun.apply(context, p1, p2, p3, null, o);
    }
}
