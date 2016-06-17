package ghcvm.runtime.stm;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class ReadTVar extends StgClosure {

    @Override
    public final void enter(StgContext context) {
        StgTVar tvar = (StgTVar) context.R1;
        context.myCapability.stmReadTvar(context.currentTSO.trec, tvar);
    }
}
