package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApF extends UpdatableThunk {
    public Closure p1;
    public float p2;

    public ApF(final Closure p1, final float p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return p1.applyF(context, p2);
    }

    @Override
    public final void clear() {
        this.p1 = null;
    }
}
