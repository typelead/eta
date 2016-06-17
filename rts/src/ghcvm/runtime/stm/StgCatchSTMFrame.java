package ghcvm.runtime.stm;

import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StgCatchSTMFrame extends StackFrame {
    public final StgClosure code;
    public final StgClosure handler;

    public StgCatchSTMFrame(final StgClosure code, final StgClosure handler) {
        this.code = code;
        this.handler = handler;
    }

    @Override
    public void stackEnter(StgContext context) {
        // TODO: Implement
    }

}
