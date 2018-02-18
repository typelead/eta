package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApF extends UpdatableThunk {
    public Closure x1;
    public float x2;

    public ApF(final Closure x1, final float x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.applyF(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
