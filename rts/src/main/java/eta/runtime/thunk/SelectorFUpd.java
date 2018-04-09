package eta.runtime.thunk;

import eta.Closure;
import eta.runtime.stg.StgContext;
import eta.DataCon;

public class SelectorFUpd extends SelectorUpd {

    public SelectorFUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        context.F1 = result.getF(index);
        return null;
    }
}
