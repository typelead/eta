package eta.runtime.thunk;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgEnter;
import eta.runtime.thunk.StgWhiteHole;
import static eta.runtime.stg.StackFrame.MarkFrameResult.UpdateEvaluted;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Update;

public class StgUpdateFrame extends UpdateFrame {

    public StgUpdateFrame(StgThunk updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        updatee.updateWithIndirection(context.R(1));
    }
}
