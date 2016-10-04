package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class Ap7Upd extends StgInd {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public StgClosure p4;
    public StgClosure p5;
    public StgClosure p6;
    public StgClosure p7;

    public Ap7Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4, final StgClosure p5, final StgClosure p6, final StgClosure p7) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
        this.p6 = p6;
        this.p7 = p7;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        context.R(3, p3);
        context.R(4, p4);
        context.R(5, p5);
        context.R(6, p6);
        context.R(7, p7);
        Apply.ap_pppppp_fast.enter(context);
    }
}
