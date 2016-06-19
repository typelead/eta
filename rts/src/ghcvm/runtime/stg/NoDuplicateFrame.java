package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgContext;

public class NoDuplicateFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Stg.noDuplicate.enter(context);
    }
}
