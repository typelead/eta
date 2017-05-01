package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgConstr;
import eta.runtime.apply.Apply;

public class SelectorDUpd extends SelectorUpd {

    public SelectorDUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void selectEnter(StgContext context) {
        context.D(1, ((StgConstr) context.R(1)).getD(index));
    }
}
