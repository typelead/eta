package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorDNoUpd extends SelectorNoUpd {

    public SelectorDNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public StgClosure selectEnter(StgContext context, StgConstr result) {
        context.D(1, result.getD(index));
        return null;
    }
}
