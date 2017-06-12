package eta.runtime.interpreter;

import java.util.ListIterator;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import eta.runtime.io.Array;
import eta.runtime.io.ByteArray;
import eta.runtime.exception.Exception;
import static eta.runtime.stg.TSO.WhatNext.ThreadInterpret;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

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
