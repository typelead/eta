package ghcvm.runtime.stm;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class StgAtomically extends StgClosure {
    public final StgClosure stmCode;

    public StgAtomically(final StgClosure stmCode) {
        this.stmCode = stmCode;
    }

    @Override
    public void enter(StgContext context) {
        context.R(1, stmCode);
        STM.atomically.enter(context);
    }
}
