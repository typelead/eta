package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class Ap7Upd extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;
    public Closure x5;
    public Closure x6;
    public Closure x7;

    public Ap7Upd(final Closure x1, final Closure x2, final Closure x3, final Closure x4, final Closure x5, final Closure x6, final Closure x7) {
        super();
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
        this.x5 = x5;
        this.x6 = x6;
        this.x7 = x7;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        final Closure x1 = this.x1;
        final Closure x2 = this.x2;
        final Closure x3 = this.x3;
        final Closure x4 = this.x4;
        final Closure x5 = this.x5;
        final Closure x6 = this.x6;
        final Closure x7 = this.x7;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.apply6(context, x2, x3, x4, x5, x6, x7);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
        this.x4 = null;
        this.x5 = null;
        this.x6 = null;
        this.x7 = null;
    }
}
