package ghcvm.runtime.concurrent;

import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;

public final class BlockPutMVarFrame extends StackFrame {
    public final StgMVar mvar;
    public final StgClosure val;

    public BlockPutMVarFrame(final StgMVar mvar, final StgClosure val) {
        this.mvar = mvar;
        this.val = val;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R1 = mvar;
        context.R2 = val;
        Concurrent.putMVar.enter(context);
    }
}
