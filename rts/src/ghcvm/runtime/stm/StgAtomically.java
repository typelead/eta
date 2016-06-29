package ghcvm.runtime.stm;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgThunk;

public class StgAtomically extends StgThunk {
    /* TODO: Should this be an Ind? */
    public final StgClosure stmCode;

    public StgAtomically(final StgClosure stmCode) {
        this.stmCode = stmCode;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R(1, stmCode);
        STM.atomically.enter(context);
    }
}
