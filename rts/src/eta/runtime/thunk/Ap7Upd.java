package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;


public class Ap7Upd extends UpdatableThunk {
    public Closure p1;
    public Closure p2;
    public Closure p3;
    public Closure p4;
    public Closure p5;
    public Closure p6;
    public Closure p7;

    public Ap7Upd(final Closure p1, final Closure p2, final Closure p3, final Closure p4, final Closure p5, final Closure p6, final Closure p7) {
        super();
        this.p1 = p1;
        this.p2 = p2;
        this.p3 = p3;
        this.p4 = p4;
        this.p5 = p5;
        this.p6 = p6;
        this.p7 = p7;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return p1.applyPPPPPP(context, p2, p3, p4, p5, p6, p7);
    }
}
