package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApD extends UpdatableThunk {
    public Closure x1;
    public double x2;

    public ApD(final Closure x1, final double x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.applyD(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
