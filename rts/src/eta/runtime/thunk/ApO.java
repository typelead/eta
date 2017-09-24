package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApO extends UpdatableThunk {
    public Closure p1;
    public Object p2;

    public ApO(final Closure p1, final Object p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyO(context, p2);
    }

    @Override
    public void clear() {
        this.p1 = null;
        this.p2 = null;
    }
}
