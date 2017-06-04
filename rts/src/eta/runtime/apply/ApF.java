package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApF extends StackFrame {
    public float f;

    public ApF(float f) {
        this.f = f;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1).applyF(context, f);
    }
}
