package eta.runtime.thunk;

import eta.Closure;
import eta.runtime.stg.StgContext;
import eta.DataCon;

public class SelectorNNoUpd extends SelectorNoUpd {

    public SelectorNNoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        context.I1 = result.getN(index);
        return null;
    }
}
