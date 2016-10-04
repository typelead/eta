package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class Ap1Upd extends StgInd {
    public StgClosure p;

    public Ap1Upd(final StgClosure p) {
        super();
        this.p = p;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p);
        Apply.ap_0_fast.enter(context);
    }
}
