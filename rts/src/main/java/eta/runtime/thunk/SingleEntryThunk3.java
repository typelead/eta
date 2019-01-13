package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class SingleEntryThunk3 extends SingleEntryThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;

    public SingleEntryThunk3(Closure x1, Closure x2, Closure x3) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }
}
