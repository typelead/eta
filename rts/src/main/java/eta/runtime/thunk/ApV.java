package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApV extends UpdatableThunk {
    public Closure x1;

    public ApV(final Closure x1) {
        super();
        this.x1 = x1;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.applyV(context);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
