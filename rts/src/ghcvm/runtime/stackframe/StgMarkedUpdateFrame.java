package ghcvm.runtime.stackframe;

import ghcvm.runtime.closure.*;

public class StgMarkedUpdateFrame extends UpdateFrame {

    public StgMarkedUpdateFrame(StgInd updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        StgClosure ret = context.R1;
        StgClosure v = updatee.indirectee;
        if (v.isEvaluated()) {
            context.myCapability.checkBlockingQueues(context.currentTSO);
            context.R1 = v;
        } else if (v == context.currentTSO) {
            updatee.updateWithIndirection(ret);
        } else {
            context.myCapability.updateThunk(context.currentTSO, updatee, ret);
            context.R1 = ret;
        }
    }

    @Override
    public MarkFrameResult mark() {
        return Marked;
    }
}
