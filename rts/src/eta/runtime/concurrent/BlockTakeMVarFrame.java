package eta.runtime.concurrent;

import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public final class BlockTakeMVarFrame extends StackFrame {
    public final StgMVar mvar;

    public BlockTakeMVarFrame(final StgMVar mvar) {
        this.mvar = mvar;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.O(1, mvar);
        Concurrent.takeMVar.enter(context);
    }
}
