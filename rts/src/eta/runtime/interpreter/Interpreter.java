package eta.runtime.interpreter;

import java.util.ListIterator;

import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.RtsFun;
import eta.runtime.exception.StgException;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadInterpret;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadYielding;

public class Interpreter {
    public static Capability interpretBCO(Capability cap) {
        ListIterator<StackFrame> sp = cap.context.currentTSO.sp;

        // TODO: Implement
        return cap;
    }

    public static RtsFun yieldToInterpreter = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                tso.whatNext = ThreadInterpret;
                context.ret = ThreadYielding;
                Interpreter.returnToSchedNotPaused.enter(context);
            }
        };

    public static RtsFun returnToSchedNotPaused = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                throw StgException.stgReturnException;
            }
        };
}
