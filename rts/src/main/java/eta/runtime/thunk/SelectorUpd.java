package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.stg.StgContext;

public abstract class SelectorUpd extends UpdatableThunk {
    protected final int index;
    protected Closure p;

    public SelectorUpd(int i, Closure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return selectEnter(context, (DataCon) p.evaluate(context));
    }

    @Override
    public final void clear() {
        this.p = null;
    }

    public abstract Closure selectEnter(StgContext context, DataCon result);
}
