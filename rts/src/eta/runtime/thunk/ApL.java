package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApL extends UpdatableThunk {
    public Closure p1;
    public long p2;

    public ApL(final Closure p1, final long p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyL(context, p2);
    }

    @Override
    public void clear() {
        this.p1 = null;
    }
}
