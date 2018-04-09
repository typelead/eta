package eta.runtime.thunk;

import eta.Closure;
import eta.UpdatableThunk;
import eta.runtime.stg.StgContext;

public class Ap5Upd extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;
    public Closure x5;

    public Ap5Upd(final Closure x1, final Closure x2, final Closure x3, final Closure x4, final Closure x5) {
        super();
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
        this.x5 = x5;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return x1.apply4(context, x2, x3, x4, x5);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
        this.x4 = null;
        this.x5 = null;
    }
}
