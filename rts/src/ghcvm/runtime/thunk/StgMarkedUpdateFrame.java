package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult;
import static ghcvm.runtime.stg.StackFrame.MarkFrameResult.Marked;

public class StgMarkedUpdateFrame extends StgGenUpdateFrame {

    public StgMarkedUpdateFrame(StgThunk updatee) {
        super(updatee);
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) {
        return Marked;
    }
}
