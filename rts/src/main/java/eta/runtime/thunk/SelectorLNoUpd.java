package eta.runtime.thunk;

import eta.Closure;
import eta.runtime.stg.StgContext;
import eta.DataCon;

public class SelectorLNoUpd extends SelectorNoUpd {

    public SelectorLNoUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        context.L1 = result.getL(index);
        return null;
    }
}
