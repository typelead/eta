package eta.runtime.interpreter;

import eta.runtime.stg.Closure;
import eta.runtime.io.StgArray;
import eta.runtime.io.StgByteArray;

public class StgBCO extends Value {
    public final StgByteArray instrs;
    public final StgByteArray literals;
    public final StgArray ptrs;
    public final int arity;
    public final int[] bitmap;

    public StgBCO(StgByteArray instrs, StgByteArray literals, StgArray ptrs,
                  int arity, int[] bitmap) {
        this.instrs = instrs;
        this.literals = literals;
        this.ptrs = ptrs;
        this.arity = arity;
        this.bitmap = bitmap;
    }
}
