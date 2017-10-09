package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class UpdatableThunk2 extends UpdatableThunk {
    public Closure x1;
    public Closure x2;

    public UpdatableThunk2(Closure x1, Closure x2) {
        this.x1 = x1;
        this.x2 = x2;
    }

    @Override
    public final void clear() {
        this.x1 = null;
        this.x2 = null;
    }
}
