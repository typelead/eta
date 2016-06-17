package ghcvm.runtime.stm;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class NewTVar extends StgClosure {

    @Override
    public final void enter(StgContext context) {
        StgClosure init = context.R1;
        context.R1 = new StgTVar(init);
    }
}
