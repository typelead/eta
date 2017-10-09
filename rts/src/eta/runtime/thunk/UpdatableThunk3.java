package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class UpdatableThunk3 extends UpdatableThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;

    public UpdatableThunk3(Closure x1, Closure x2, Closure x3) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
        this.x3 = null;
    }
}
