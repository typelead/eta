package eta.runtime.interpreter;

import java.util.ListIterator;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import eta.runtime.io.StgArray;
import eta.runtime.io.StgByteArray;
import eta.runtime.exception.StgException;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadInterpret;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Interpreter {

    public static Capability interpretBCO(Capability cap) {
        ListIterator<StackFrame> sp = cap.context.currentTSO.sp;
        // TODO: Implement
        return cap;
    }

    public static void yieldToInterpreter(StgContext context) {
        StgTSO tso = context.currentTSO;
        tso.whatNext = ThreadInterpret;
        context.ret = ThreadYielding;
        returnToSchedNotPaused(context);
    }

    public static void returnToSchedNotPaused(StgContext context) {
        throw StgException.stgReturnException;
    }

    /* TODO: Inline this */
    public static void newBCO(StgContext context, StgByteArray instrs, StgByteArray literals, StgArray ptrs, int arity, StgByteArray bitmap) {
        /* TODO: Copy the bitmap */;
        context.O(1, new StgBCO(instrs, literals, ptrs, arity, null));
    }
}
