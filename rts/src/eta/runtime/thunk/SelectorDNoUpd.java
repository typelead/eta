package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.DataConstructor;


public class SelectorDNoUpd extends SelectorNoUpd {

    public SelectorDNoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataConstructor result) {
        context.D(1, result.getD(index));
        return null;
    }
}
