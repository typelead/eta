package eta.runtime.concurrent;

import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public final class BlockPutMVarFrame extends StackFrame {
    public final StgMVar mvar;
    public final StgClosure val;

    public BlockPutMVarFrame(final StgMVar mvar, final StgClosure val) {
        this.mvar = mvar;
        this.val = val;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.R(1, mvar);
        context.R(2, val);
        Concurrent.putMVar.enter(context);
    }
}
