package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Marked;

public class StgMarkedUpdateFrame extends UpdateFrame {

    public StgMarkedUpdateFrame(StgThunk updatee) {
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

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) {
        return Marked;
    }
}
