package eta.runtime.concurrent;

import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgContext;

public final class BlockReadMVarFrame extends StackFrame {
    public final StgMVar mvar;

    public BlockReadMVarFrame(final StgMVar mvar) {
        this.mvar = mvar;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.O(1, mvar);
        Concurrent.readMVar.enter(context);
    }
}
