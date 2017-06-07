package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;


public class Ap3Upd extends StgInd {
    public StgClosure p1;
    public StgClosure p2;
    public StgClosure p3;

    public Ap3Upd(final StgClosure p1, final StgClosure p2, final StgClosure p3) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
    }

    @Override
    public StgClosure thunkEnter(StgContext context) {
        return p1.applyPP(context, p2, p3);
    }
}
