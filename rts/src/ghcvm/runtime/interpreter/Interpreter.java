package ghcvm.runtime.interpreter;

import ghcvm.runtime.stg.Capability;

public class Interpreter {
    public static Capability interpretBCO(Capability cap) {
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
