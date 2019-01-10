package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

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
        final Closure x1 = this.x1;
        final Object  x2 = this.x2;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.applyO(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
    }
}
