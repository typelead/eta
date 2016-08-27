package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.Apply;

public class SelectorNNoUpd extends SelectorNoUpd {

    public SelectorNNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.I(1, ((StgConstr) context.R(1)).getI(index));
    }
}
