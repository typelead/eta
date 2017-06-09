package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.DataConstructor;


public class SelectorPUpd extends SelectorUpd {

    public SelectorPUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataConstructor result) {
        return result.getP(index).evaluate(context);
    }
}
