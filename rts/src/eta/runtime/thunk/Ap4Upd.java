package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.Apply;

public class Ap4Upd extends StgInd {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;
    public StgClosure p4;

    public Ap4Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3, final StgClosure p4) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
    }

    @Override
    public void thunkEnter(StgContext context) {
        p1.applyPPP(context, p2, p3, p4);
    }
}
