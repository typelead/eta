package eta.runtime.exception;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public class BlockThrowToFrame extends StackFrame {

    public final StgTSO tso;
    public final StgClosure exception;

    public BlockThrowToFrame(final StgTSO tso, final StgClosure exception) {
        this.tso = tso;
        this.exception = exception;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.O(1, tso);
        context.R(1, exception);
        StgException.killThread.enter(context);
    }
}
