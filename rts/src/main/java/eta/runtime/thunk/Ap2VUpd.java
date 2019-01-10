package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;

public class Ap2VUpd extends UpdatableThunk {
    public Closure x1;
    public Closure x2;

    public Ap2VUpd(final Closure x1, final Closure x2) {
        super();
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        final Closure x1 = this.x1;
        final Closure x2 = this.x2;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else return x1.apply1V(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
    }
}
