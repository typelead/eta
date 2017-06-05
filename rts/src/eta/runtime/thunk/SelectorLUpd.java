package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorLUpd extends SelectorUpd {

    public SelectorLUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void selectEnter(StgContext context) {
        context.L(1, ((StgConstr) context.R(1)).getL(index));
    }
}
