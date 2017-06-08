package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorFUpd extends SelectorUpd {

    public SelectorFUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, StgConstr result) {
        context.F(1, result.getF(index));
        return null;
    }
}
