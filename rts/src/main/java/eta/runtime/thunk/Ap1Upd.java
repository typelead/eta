package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Ap1Upd extends UpdatableThunk {
    public Closure p;

    public Ap1Upd(final Closure p) {
        super();
        this.p = p;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return p.evaluate(context);
    }

    @Override
    public final void clear() {
        this.p = null;
    }
}
