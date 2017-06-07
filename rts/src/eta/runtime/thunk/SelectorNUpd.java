package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorNUpd extends SelectorUpd {

    public SelectorNUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public StgClosure selectEnter(StgContext context, StgConstr result) {
        context.I(1, result.getN(index));
        return null;
    }
}
