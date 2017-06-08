package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorLNoUpd extends SelectorNoUpd {

    public SelectorLNoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, StgConstr result) {
        context.L(1, result.getL(index));
        return null;
    }
}
