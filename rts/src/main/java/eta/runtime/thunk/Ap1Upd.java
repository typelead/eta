package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Ap1Upd extends UpdatableThunk {
    public Closure x1;

    public Ap1Upd(final Closure x1) {
        super();
        this.x1 = x1;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.evaluate(context);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
