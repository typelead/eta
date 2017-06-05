package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorPUpd extends SelectorUpd {

    public SelectorPUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void selectEnter(StgContext context) {
        ((StgConstr) context.R(1)).getP(index).evaluate(context);
    }
}
