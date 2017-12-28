package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Ap1VUpd extends UpdatableThunk {
    public Closure p;

    public Ap1VUpd(final Closure p) {
        super();
        this.p = p;
    }

    @Override
    public final Closure thunkEnter(StgContext context) {
        return p.applyV(context);
    }

    @Override
    public final void clear() {
        this.p = null;
    }
}
