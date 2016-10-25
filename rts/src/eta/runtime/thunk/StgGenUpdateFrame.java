package eta.runtime.thunk;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;

public class StgGenUpdateFrame extends UpdateFrame {

    public StgGenUpdateFrame(StgThunk updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure ret = context.R(1);
        StgClosure v = updatee.indirectee;
        if (v.getEvaluated() != null) {
            context.myCapability.checkBlockingQueues(context.currentTSO);
            context.R(1, v);
        } else if (v == context.currentTSO) {
            updatee.updateWithIndirection(ret);
        } else {
            context.myCapability.updateThunk(context.currentTSO, updatee, ret);
            context.R(1, ret);
        }
    }
}
