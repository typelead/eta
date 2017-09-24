package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApI extends UpdatableThunk {
    public Closure p1;
    public int p2;

    public ApI(final Closure p1, final int p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyN(context, p2);
    }

    @Override
    public void clear() {
        this.p1 = null;
    }
}
