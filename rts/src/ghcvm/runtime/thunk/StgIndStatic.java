package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.util.UnsafeUtil;
import ghcvm.runtime.thunk.StgWhiteHole;

public class StgIndStatic extends StgThunk {

    public StgIndStatic() {
        super();
    }

    public StgIndStatic(StgClosure indirectee) {
        super(indirectee);
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
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
            context.R(1, this);
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
