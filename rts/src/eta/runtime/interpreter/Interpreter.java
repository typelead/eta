package eta.runtime.interpreter;

import eta.runtime.stg.Closure;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgContext;
import eta.runtime.io.Array;
import eta.runtime.io.ByteArray;

public class Interpreter {

    public static Capability interpretBCO(Capability cap) {
        barf("Interpreter not implemented yet.");
        // TODO: Implement
        return cap;
    }

    /* TODO: Inline this */
    public static Closure newBCO(StgContext context, ByteArray instrs, ByteArray literals, Array ptrs, int arity, ByteArray bitmap) {
        /* TODO: Copy the bitmap */;
        context.O(1, new BCO(instrs, literals, ptrs, arity, null));
        return null;
    }
}
