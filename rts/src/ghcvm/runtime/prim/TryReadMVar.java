package ghcvm.runtime.prim;

import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.closure.StgClosure;

public class TryReadMVar extends StgClosure {
    @Override
    public void enter(StgContext context) {
        StgMVar mvar = (StgMVar) context.R1;
        // Use a more lightweight blocking mechanism?
        // This synchronisation will not have thread yields
        synchronized (mvar) {
            StgClosure value = mvar.value;
            if (value == null) {
                context.I1 = 0;
                context.R1 = null;
            } else {
                context.I1 = 1;
                context.R1 = value;
            }
        }
    }
}
