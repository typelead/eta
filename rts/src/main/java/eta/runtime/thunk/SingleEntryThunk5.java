package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class SingleEntryThunk5 extends SingleEntryThunk {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;
    public Closure x5;

    public SingleEntryThunk5(Closure x1, Closure x2, Closure x3, Closure x4, Closure x5) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
        this.x5 = x5;
    }
}
