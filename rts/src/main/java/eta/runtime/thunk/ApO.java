package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApO extends UpdatableThunk {
    public Closure x1;
    public Object x2;

    public ApO(final Closure x1, final Object x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.applyO(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
    }
}
