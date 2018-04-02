package eta.runtime.thunk;

import eta.Closure;
import eta.UpdatableThunk;
import eta.runtime.stg.StgContext;

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
        return x1.apply1V(context, x2);
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
    }
}
