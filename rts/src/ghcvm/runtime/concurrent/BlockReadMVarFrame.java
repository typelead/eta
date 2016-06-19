package ghcvm.runtime.concurrent;

import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgContext;

public final class BlockReadMVarFrame extends StackFrame {
    public final StgMVar mvar;

    public BlockReadMVarFrame(final StgMVar mvar) {
        this.mvar = mvar;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R1 = mvar;
        Concurrent.readMVar.enter(context);
    }
}
