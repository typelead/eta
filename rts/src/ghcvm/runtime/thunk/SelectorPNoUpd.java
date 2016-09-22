package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgConstr;
import ghcvm.runtime.apply.Apply;

public class SelectorPNoUpd extends SelectorNoUpd {

    public SelectorPNoUpd(int i, StgClosure p) {
        super(i, p);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R(1, ((StgConstr) context.R(1)).getP(index));
        Apply.ap_0_fast.enter(context);
    }
}
