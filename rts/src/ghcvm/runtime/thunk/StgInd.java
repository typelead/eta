package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgPayload;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgWhiteHole;
import ghcvm.runtime.message.MessageBlackHole;
import ghcvm.runtime.util.UnsafeUtil;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;

public abstract class StgInd extends StgThunk {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        if (indirectee == null) {
            StgTSO tso = context.currentTSO;
            tso.sp.add(new StgUpdateFrame(this));
            thunkEnter(context);
        } else {
            Thunk.blackHole(context, this, indirectee);
        }
    }
}
