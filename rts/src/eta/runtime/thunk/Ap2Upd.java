package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.Apply;

public class Ap2Upd extends StgInd {
    public StgClosure p1;
    public StgClosure p2;

    public Ap2Upd(final StgClosure p1, final StgClosure p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public void thunkEnter(StgContext context) {
        context.R(1, p1);
        context.R(2, p2);
        Apply.ap_p_fast.enter(context);
    }
}
