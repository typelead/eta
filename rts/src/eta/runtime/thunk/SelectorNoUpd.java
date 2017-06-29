package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.stg.StgContext;

public abstract class SelectorNoUpd extends Thunk {
    protected final int index;
    protected final Closure p;

    public SelectorNoUpd(int i, Closure p) {
        super();
        this.index = i;
        this.p = p;
    }

    @Override
    public final Closure enter(StgContext context) {
        return selectEnter(context, (DataCon) p.evaluate(context));
    }

    public abstract Closure selectEnter(StgContext context, DataCon result);
}
