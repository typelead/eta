package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorNNoUpd extends SelectorNoUpd {

    public SelectorNNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public StgClosure selectEnter(StgContext context, StgConstr result) {
        context.I(1, result.getN(index));
        return null;
    }
}
