package ghcvm.runtime.stackframe;

import ghcvm.runtime.closure.*;

public class StgUpdateFrame extends UpdateFrame {
    public boolean marked;

    public StgUpdateFrame(StgInd updatee) {
        super(updatee);
    }

    public StgUpdateFrame(StgInd updatee, boolean marked) {
        this(updatee);
        this.marked = marked;
    }

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        StgClosure ret = context.R1;
        if (marked) {
            StgClosure v = updatee.indirectee;
            if (v.isEvaluated()) {
                context.myCapability.checkBlockingQueues(context.currentTSO);
                context.R1 = v;
            } else if (v == context.currentTSO) {
                updatee.updateWithIndirection(ret);
                context.R1 = ret;
            } else {
                context.myCapability.updateThunk(context.currentTSO, updatee, ret);
                context.R1 = ret;
            }
        } else {
            updatee.updateWithIndirection(ret);
            context.R1 = ret;
        }
    }
}
