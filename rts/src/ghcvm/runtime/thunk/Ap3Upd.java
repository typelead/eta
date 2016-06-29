package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class Ap3Upd extends StgInd {
    private final StgClosure p1;
    private final StgClosure p2;
    private final StgClosure p3;

    public Ap3Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        context.R(3, p3);
        Apply.ap_pp_fast.enter(context);
    }
}
