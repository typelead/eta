package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;


public class Ap5Upd extends UpdatableThunk {
    public Closure p1;
    public Closure p2;
    public Closure p3;
    public Closure p4;
    public Closure p5;

    public Ap5Upd(final Closure p1, final Closure p2, final Closure p3, final Closure p4, final Closure p5) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyPPPP(context, p2, p3, p4, p5);
    }
}
