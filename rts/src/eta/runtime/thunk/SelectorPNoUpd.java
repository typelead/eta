package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorPNoUpd extends SelectorNoUpd {

    public SelectorPNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public StgClosure selectEnter(StgContext context, StgConstr result) {
        return result.getP(index).evaluate(context);
    }
}
