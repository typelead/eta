package ghcvm.runtime.stg;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.stackframe.StackFrame;

public class ForceIO extends StackFrame {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R1.enter(context);
    }
}
