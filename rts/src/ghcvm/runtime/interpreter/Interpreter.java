package ghcvm.runtime.interpreter;

import java.util.ListIterator;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.RtsFun;
import ghcvm.runtime.exception.StgException;
import static ghcvm.runtime.stg.StgTSO.WhatNext.ThreadInterpret;
import static ghcvm.runtime.stg.StgContext.ReturnCode.ThreadYielding;

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
