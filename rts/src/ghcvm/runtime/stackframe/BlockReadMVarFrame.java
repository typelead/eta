package ghcvm.runtime.stackframe;

import ghcvm.runtime.Stg;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.prim.StgMVar;

public final class BlockReadMVarFrame extends StackFrame {
    public final StgMVar mvar;

    public BlockReadMVarFrame(final StgMVar mvar) {
        this.mvar = mvar;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        context.R1 = mvar;
        Stg.readMVar.enter(context);
    }
}
