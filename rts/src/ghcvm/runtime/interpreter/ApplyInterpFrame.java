package ghcvm.runtime.interpreter;

import ghcvm.runtime.stg.StgBCO;

public class ApplyInterpFrame extends StackFrame {
    public final StgBCO bco;

    public ApplyInterpFrame(final StgBCO bco) {
        this.bco = bco;
    }

    @Override
    public void stackEnter(StgContext context) {
        Interpreter.yieldToInterpreter.enter(context);
    }
}
