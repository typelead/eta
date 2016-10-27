package eta.runtime.interpreter;

import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;

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
