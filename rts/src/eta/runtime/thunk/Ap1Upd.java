package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;


public class Ap1Upd extends StgInd {
    public StgClosure p;

    public Ap1Upd(final StgClosure p) {
        super();
        this.p = p;
    }

    @Override
    public StgClosure thunkEnter(StgContext context) {
        return p.evaluate(context);
    }
}
