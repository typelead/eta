package ghcvm.runtime.stackframe;

import ghcvm.runtime.Stg;
import ghcvm.runtime.closure.StgContext;

public class NoDuplicateFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Stg.noDuplicate.enter(context);
    }
}
