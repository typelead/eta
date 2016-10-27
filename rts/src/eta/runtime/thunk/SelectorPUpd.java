package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;
import eta.runtime.apply.Apply;

public class SelectorPUpd extends SelectorUpd {

    public SelectorPUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void thunkEnter(StgContext context) {
        super.thunkEnter(context);
        context.R(1, ((StgConstr) context.R(1)).getP(index));
        Apply.ap_0_fast.enter(context);
    }
}
