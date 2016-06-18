package ghcvm.runtime.stm;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StgAtomically extends StgClosure {
    public final StgClosure stmCode;

    public StgAtomically(final StgClosure stmCode) {
        this.stmCode = stmCode;
    }

    @Override
    public void enter(StgContext context) {
        context.R1 = stmCode;
        STM.atomically.enter(context);
    }
}
