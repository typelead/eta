package ghcvm.runtime.concurrent;

import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;

public final class BlockTakeMVarFrame extends StackFrame {
    public final StgMVar mvar;

    public BlockTakeMVarFrame(final StgMVar mvar) {
        this.mvar = mvar;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1, mvar);
        Concurrent.takeMVar.enter(context);
    }
}
