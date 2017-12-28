package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Ap2VUpd extends UpdatableThunk {
    public Closure p1;
    public Closure p2;

    public Ap2VUpd(final Closure p1, final Closure p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return p1.apply1V(context, p2);
    }

    @Override
    public final void clear() {
        this.p1 = null;
        this.p2 = null;
    }
}
