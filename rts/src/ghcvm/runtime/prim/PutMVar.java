package ghcvm.runtime.prim;

import ghcvm.runtime.Stg;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.StgTSO.WhyBlocked;
import ghcvm.runtime.prim.StgMVar;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.stackframe.ReturnClosure;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.*;

public class PutMVar extends StgClosure {
    @Override
    public void enter(StgContext context) {
        StgMVar mvar = (StgMVar) context.R1;
        mvar.lock();
        StgClosure val = context.R2;
        StgTSO tso;
        if (mvar.value != null) {
            tso = context.currentTSO;
            tso.blockInfo = mvar;
            tso.whyBlocked = BlockedOnMVar;
            mvar.pushLast(tso);
            context.R1 = mvar;
            context.R2 = val;
            Stg.block_putmvar.enter(context);
        } else {
            tso = mvar.popFromQueue();
            if (tso == null) {
                mvar.value = val;
                // return ()
            } else {
                WhyBlocked whyBlocked = tso.whyBlocked;
                // Is this pop actually necessary?
                // TODO: Redo stack
                // tso.stack.pop();
                // tso.stack.push(new ReturnClosure(val));
                tso.inMVarOperation = false;
                context.myCapability.tryWakeupThread(tso);
                if (whyBlocked == BlockedOnMVarRead) {
                    // TODO: check this condition if it's valid
                }
                // return ()
            }
        }
        mvar.unlock();
    }
}
