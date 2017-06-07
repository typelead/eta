package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;

public abstract class SelectorUpd extends StgInd {
    protected final int index;
    protected final StgClosure p;

    public SelectorUpd(int i, StgClosure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public final void thunkEnter(StgContext context) {
        selectEnter(context, (StgConstr) p.evaluate(context));
    }

    public abstract void selectEnter(StgContext context, StgConstr result);
}
