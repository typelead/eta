package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.DataCon;


public class SelectorPUpd extends SelectorUpd {

    public SelectorPUpd(int i, Closure p) {
        super(i, p);
    }

    @Override
    public Closure selectEnter(StgContext context, DataCon result) {
        return result.getP(index).evaluateTail(context);
    }
}
