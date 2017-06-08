package eta.runtime.stm;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.Thunk;

public class StgAtomically extends Thunk {
    /* TODO: Should this be an Ind? */
    public final Closure stmCode;

    public StgAtomically(final Closure stmCode) {
        this.stmCode = stmCode;
    }

    @Override
    public Closure enter(StgContext context) {
        context.R(1, this); // TODO: Verify
        return STM.atomically(context, stmCode);
    }
}
