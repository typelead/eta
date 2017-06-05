package eta.runtime.stm;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.StgThunk;

public class StgAtomically extends StgThunk {
    /* TODO: Should this be an Ind? */
    public final StgClosure stmCode;

    public StgAtomically(final StgClosure stmCode) {
        this.stmCode = stmCode;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        STM.atomically(context, stmCode);
    }
}
