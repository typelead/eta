package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApV extends UpdatableThunk {
    public Closure p;

    public ApV(final Closure p) {
        super();
        this.p = p;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p.applyV(context);
    }
}
