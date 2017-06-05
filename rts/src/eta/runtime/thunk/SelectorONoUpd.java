package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorONoUpd extends SelectorNoUpd {

    public SelectorONoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void selectEnter(StgContext context) {
        context.O(1, ((StgConstr) context.R(1)).getO(index));
    }
}
