package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.Apply;

public class SelectorOUpd extends SelectorUpd {

    public SelectorOUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void thunkEnter(StgContext context) {
        super.thunkEnter(context);
        context.O(1, ((StgConstr) context.R(1)).getO(index));
    }
}
