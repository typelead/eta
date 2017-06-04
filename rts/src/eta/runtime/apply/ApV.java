package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public class ApV extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        context.R(1).applyV(context);
    }
}
