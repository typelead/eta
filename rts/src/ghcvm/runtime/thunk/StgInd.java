package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgPayload;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.thunk.StgWhiteHole;
import ghcvm.runtime.message.MessageBlackHole;
import ghcvm.runtime.util.UnsafeUtil;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;

public abstract class StgInd extends StgClosure {
    public volatile StgClosure indirectee;

    public StgInd(StgClosure indirectee) {
        this.indirectee = indirectee;
    }

    public abstract void thunkEnter(StgContext context);

    @Override
    public void enter(StgContext context) {
        if (indirectee != null) {
            retry: do {
                if (indirectee.isEvaluated()) {
                    context.R1 = indirectee;
                } else {
                    StgTSO currentTSO = context.currentTSO;
                    MessageBlackHole msg = new MessageBlackHole(currentTSO, this);
                    boolean blocked = context.myCapability.messageBlackHole(msg);
                    if (blocked) {
                        currentTSO.whyBlocked = BlockedOnBlackHole;
                        currentTSO.blockInfo = msg;
                        context.R1 = this;
                        Thunk.block_blackhole.enter(context);
                    } else {
                        continue retry;
                    }
                }
                break;
            } while (true);
        } else {
            thunkEnter(context);
        }
    }

    public void updateWithIndirection(StgClosure ret) {
        indirectee = ret;
    }

    @Override
    public final boolean isEvaluated() {
         return indirectee.isEvaluated();
    }

    public final boolean tryLock(StgClosure oldIndirectee) {
        return cas(oldIndirectee, StgWhiteHole.closure);
    }

    public final boolean cas(StgClosure expected, StgClosure update) {
        return UnsafeUtil.cas(this, expected, update);
    }
}
