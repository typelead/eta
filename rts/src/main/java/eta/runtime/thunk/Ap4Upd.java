package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class Ap4Upd extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;

    public Ap4Upd(final Closure x1, final Closure x2, final Closure x3, final Closure x4) {
        super();
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        final Closure x1 = this.x1;
        final Closure x2 = this.x2;
        final Closure x3 = this.x3;
        final Closure x4 = this.x4;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.apply3(context, x2, x3, x4);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
        this.x4 = null;
    }
}
