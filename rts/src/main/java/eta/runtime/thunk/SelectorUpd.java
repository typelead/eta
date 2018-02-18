package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.stg.StgContext;

public abstract class SelectorUpd extends UpdatableThunk {
    protected final int index;
    public Closure x1;

    public SelectorUpd(int i, Closure x1) {
        super();
        this.index = i;
        this.x1 = x1;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return selectEnter(context, (DataCon) x1.evaluate(context));
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }

    public abstract Closure selectEnter(StgContext context, DataCon result);
}
