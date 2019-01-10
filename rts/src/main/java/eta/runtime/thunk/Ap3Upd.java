package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class Ap3Upd extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;

    public Ap3Upd(final Closure x1, final Closure x2, final Closure x3) {
        super();
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        final Closure x1 = this.x1;
        final Closure x2 = this.x2;
        final Closure x3 = this.x3;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.apply2(context, x2, x3);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
    }
}
