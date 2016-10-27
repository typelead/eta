package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgPayload;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.StgWhiteHole;
import eta.runtime.message.MessageBlackHole;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;

public abstract class StgInd extends StgThunk {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        if (indirectee == null) {
            context.pushFrame(new StgUpdateFrame(this));
            thunkEnter(context);
        } else {
            Thunk.blackHole(context, this, indirectee);
        }
    }
}
