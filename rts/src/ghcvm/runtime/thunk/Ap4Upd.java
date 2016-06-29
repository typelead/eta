package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class Ap4Upd extends StgInd {
    private final StgClosure p1;
    private final StgClosure p2;
    private final StgClosure p3;
    private final StgClosure p4;

    public Ap4Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        context.R(3, p3);
        context.R(4, p4);
        Apply.ap_ppp_fast.enter(context);
    }
}
