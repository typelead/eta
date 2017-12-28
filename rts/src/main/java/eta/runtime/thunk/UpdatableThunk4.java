package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class UpdatableThunk4 extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;

    public UpdatableThunk4(Closure x1, Closure x2, Closure x3, Closure x4) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
    }


    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
        this.x4 = null;
    }
}
