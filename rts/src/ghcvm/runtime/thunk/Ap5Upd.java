package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class Ap5Upd extends StgInd {
    private final StgClosure p1;
    private final StgClosure p2;
    private final StgClosure p3;
    private final StgClosure p4;
    private final StgClosure p5;

    public Ap5Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4, final StgClosure p5) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        context.R(3, p3);
        context.R(4, p4);
        context.R(5, p5);
        Apply.ap_pppp_fast.enter(context);
    }
}
