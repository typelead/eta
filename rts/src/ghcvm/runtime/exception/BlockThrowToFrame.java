package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public class BlockThrowToFrame extends StackFrame {

    public final StgTSO tso;
    public final StgClosure exception;

    public BlockThrowToFrame(final StgTSO tso, final StgClosure exception) {
        this.tso = tso;
        this.exception = exception;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R1 = tso;
        context.R2 = exception;
        StgException.killThread.enter(context);
    }
}
