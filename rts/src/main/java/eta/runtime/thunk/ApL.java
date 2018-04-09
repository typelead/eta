package eta.runtime.thunk;

import eta.Closure;
import eta.UpdatableThunk;
import eta.runtime.stg.StgContext;

public class ApL extends UpdatableThunk {
    public Closure x1;
    public long x2;

    public ApL(final Closure x1, final long x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.applyL(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
