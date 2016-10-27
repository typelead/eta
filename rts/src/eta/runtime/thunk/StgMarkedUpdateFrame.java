package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.stg.StackFrame.MarkFrameResult;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Marked;

public class StgMarkedUpdateFrame extends StgGenUpdateFrame {

    public StgMarkedUpdateFrame(StgThunk updatee) {
        super(updatee);
    }

    @Override
    public MarkFrameResult mark(Capability cap, StgTSO tso) {
        return Marked;
    }
}
