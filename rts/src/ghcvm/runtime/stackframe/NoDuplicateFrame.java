package ghcvm.runtime.stackframe;

import ghcvm.runtime.Stg;
import ghcvm.runtime.closure.StgContext;

public class NoDuplicateFrame extends StackFrame {
    @Override
    public void enter(StgContext context) {
        super.enter(context);
        Stg.noDuplicate.enter(context);
    }
}
