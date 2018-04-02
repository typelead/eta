package eta.runtime.thunk;

import eta.Closure;
import eta.UpdatableThunk;

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
