package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Marked;

public class StgBHUpdateFrame extends StgMarkedUpdateFrame {
    public StgBHUpdateFrame(StgThunk updatee) {
        super(updatee);
    }
}
