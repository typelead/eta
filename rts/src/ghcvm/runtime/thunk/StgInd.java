package ghcvm.runtime.thunk;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgPayload;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.message.MessageBlackHole;
import static ghcvm.runtime.thunk.StgWhiteHole.stgWhiteHole;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;

public abstract class StgInd extends StgClosure {
    public static final StgPayload emptyPayload = new StgPayload();
    public volatile StgClosure indirectee;
    public StgPayload payload = emptyPayload;
    private static final AtomicReferenceFieldUpdater<StgInd, StgClosure> indirecteeUpdater = AtomicReferenceFieldUpdater.newUpdater(StgInd.class, StgClosure.class, "indirectee");

    public StgInd(StgClosure indirectee) {
        this.indirectee = indirectee;
    }

    public abstract void thunkEnter(StgContext context);

    @Override
    public void enter(StgContext context) {
        if (payload == null) {
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
        payload = null;
    }

    @Override
    public final boolean isEvaluated() {
         return indirectee.isEvaluated();
    }

    public final boolean tryLock(StgClosure oldIndirectee) {
        return indirecteeUpdater.compareAndSet(this, oldIndirectee, stgWhiteHole);
    }
}
