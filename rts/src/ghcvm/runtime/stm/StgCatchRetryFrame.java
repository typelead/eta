package ghcvm.runtime.stm;

import ghcvm.runtime.stackframe.StackFrame;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public class StgCatchRetryFrame extends StackFrame {
    public int runningAltCode;
    public final StgClosure firstCode;
    public final StgClosure altCode;

    public StgCatchRetryFrame(final StgClosure firstCode, final StgClosure altCode) {
        this.firstCode = firstCode;
        this.altCode = altCode;
    }

    @Override
    public void stackEnter(StgContext context) {
        // TODO: Implement
    }

}
