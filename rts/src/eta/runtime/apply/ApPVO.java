package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApPVO extends StackFrame {
    public StgClosure p;
    public Object o;

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
