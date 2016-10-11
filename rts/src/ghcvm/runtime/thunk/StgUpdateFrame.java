package ghcvm.runtime.thunk;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgEnter;
import ghcvm.runtime.thunk.StgWhiteHole;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.UpdateEvaluted;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Update;

public class StgUpdateFrame extends UpdateFrame {

    public StgUpdateFrame(StgThunk updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        updatee.updateWithIndirection(context.R(1));
    }
}
