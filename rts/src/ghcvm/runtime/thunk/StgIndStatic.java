package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgWhiteHole;
import ghcvm.runtime.util.UnsafeUtil;

public abstract class StgIndStatic extends StgThunk {

    @Override
    public void enter(StgContext context) {
        if (indirectee == null) {
            Capability cap = context.myCapability;
            StgThunk bh = cap.newCAF(this);
            if (bh == null) {
                enter(context);
            } else {
                StgTSO tso = context.currentTSO;
                tso.sp.add(new StgBHUpdateFrame(bh));
                thunkEnter(context);
            }
        } else {
            context.R(1, indirectee);
            indirectee.enter(context);
        }
    }

    public final boolean tryLock(StgClosure oldIndirectee) {
        return cas(oldIndirectee, StgWhiteHole.closure);
    }

    public final boolean cas(StgClosure expected, StgClosure update) {
        return UnsafeUtil.cas(this, expected, update);
    }
}
