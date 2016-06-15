package ghcvm.runtime.prim;

import ghcvm.runtime.Stg;
import ghcvm.runtime.prim.StgMVar;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgContext;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.BlockedOnMVarRead;

public class ReadMVar extends StgClosure {

    @Override
    public void enter(StgContext context) {
        StgMVar mvar = (StgMVar) context.R1;
        mvar.lock();
        if (mvar.value == null) {
            StgTSO tso = context.currentTSO;
            tso.blockInfo = mvar;
            tso.whyBlocked = BlockedOnMVarRead;
            tso.inMVarOperation = true;
            mvar.pushFirst(tso);
            context.R1 = mvar;
            Stg.block_readmvar.enter(context);
        } else {
            context.R1 = mvar.value;
        }
        mvar.unlock();
    }
}
