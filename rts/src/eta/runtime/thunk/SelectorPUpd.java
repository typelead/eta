package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;


public class SelectorPUpd extends SelectorUpd {

    public SelectorPUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, StgConstr result) {
        return result.getP(index).evaluate(context);
    }
}
