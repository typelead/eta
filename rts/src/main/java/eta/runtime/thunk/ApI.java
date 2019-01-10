package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class ApI extends UpdatableThunk {
    public Closure x1;
    public int x2;

    public ApI(final Closure x1, final int x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        final Closure x1 = this.x1;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.applyN(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
