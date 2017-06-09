package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.DataConstructor;


public class SelectorNNoUpd extends SelectorNoUpd {

    public SelectorNNoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataConstructor result) {
        context.I(1, result.getN(index));
        return null;
    }
}
