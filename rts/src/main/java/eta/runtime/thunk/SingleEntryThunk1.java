package eta.runtime.thunk;

import eta.Closure;

public abstract class SingleEntryThunk1 extends SingleEntryThunk {
    public Closure x1;

    public SingleEntryThunk1(Closure x1) {
        this.x1 = x1;
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
