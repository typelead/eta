package eta.runtime.interpreter;

import java.util.ListIterator;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.io.StgArray;
import eta.runtime.io.StgByteArray;
import eta.runtime.exception.StgException;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadInterpret;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Interpreter {
    public static RtsFun yieldToInterpreter = new YieldToInterpreter();
    public static RtsFun returnToSchedNotPaused = new ReturnToSchedNotPaused();
    public static RtsFun newBCO = new NewBCO();

    public static Capability interpretBCO(Capability cap) {
        ListIterator<StackFrame> sp = cap.context.currentTSO.sp;

        // TODO: Implement
        return cap;
    }

    private static class YieldToInterpreter extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgTSO tso = context.currentTSO;
            tso.whatNext = ThreadInterpret;
            context.ret = ThreadYielding;
            Interpreter.returnToSchedNotPaused.enter(context);
        }
    }

    private static class ReturnToSchedNotPaused extends RtsFun {
        @Override
        public void enter(StgContext context) {
            context.save = true;
        }
    }

    /* TODO: Inline this */
    private static class NewBCO extends RtsFun {
        @Override
        public void enter(StgContext context) {
            StgByteArray instrs   = (StgByteArray) context.O(1);
            StgByteArray literals = (StgByteArray) context.O(2);
            StgArray ptrs = (StgArray) context.O(3);
            int arity = context.I(1);
            StgByteArray bitmap = (StgByteArray) context.O(4);
            /* TODO: Copy the bitmap */;
            context.O(1, new StgBCO(instrs, literals, ptrs, arity, null));
        }
    }
}
