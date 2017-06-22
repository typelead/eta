package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class ApD extends UpdatableThunk {
    public Closure p1;
    public double p2;

    public Ap2Upd(final Closure p1, final double p2) {
        super();
        this.p1 = p1;
        this.p2 = p2;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyD(context, p2);
    }
}
