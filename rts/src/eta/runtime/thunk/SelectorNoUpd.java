package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;


public abstract class SelectorNoUpd extends StgThunk {
    protected final int index;
    protected final StgClosure p;

    public SelectorNoUpd(int i, StgClosure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public final StgClosure enter(StgContext context) {
        return selectEnter(context, (StgConstr) p.evaluate(context));
    }

    public abstract StgClosure selectEnter(StgContext context, StgConstr result);
}
