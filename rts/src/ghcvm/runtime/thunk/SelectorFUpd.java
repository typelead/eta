package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.Apply;

public class SelectorFUpd extends SelectorUpd {

    public SelectorFUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void thunkEnter(StgContext context) {
        super.thunkEnter(context);
        context.F(1, ((StgConstr) context.R(1)).getF(index));
    }
}
