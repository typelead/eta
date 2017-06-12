package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.DataCon;


public class SelectorONoUpd extends SelectorNoUpd {

    public SelectorONoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        context.O(1, result.getO(index));
        return null;
    }
}
