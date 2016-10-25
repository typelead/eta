package eta.runtime.stg;

import eta.runtime.stg.StgContext;

public class NoDuplicateFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Stg.noDuplicate.enter(context);
    }
}
