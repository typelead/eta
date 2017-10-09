package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class UpdatableThunk1 extends UpdatableThunk {
    public Closure x1;

    public UpdatableThunk1(Closure x1) {
        this.x1 = x1;
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
