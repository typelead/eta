package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Apply;

public class SelectorUpd extends StgInd {
    protected final int index;
    protected final StgClosure p;

    public SelectorUpd(int i, StgClosure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public void thunkEnter(StgContext context) {
        p.evaluate(context);
    }
}
