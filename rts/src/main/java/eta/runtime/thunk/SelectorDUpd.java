package eta.runtime.thunk;

import eta.Closure;
import eta.runtime.stg.StgContext;
import eta.DataCon;

public class SelectorDUpd extends SelectorUpd {

    public SelectorDUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        context.D1 = result.getD(index);
        return null;
    }
}
