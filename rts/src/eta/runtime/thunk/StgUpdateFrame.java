package eta.runtime.thunk;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgEnter;
import eta.runtime.thunk.StgWhiteHole;
import static eta.runtime.stg.StackFrame.MarkFrameResult.UpdateEvaluted;
import static eta.runtime.stg.StackFrame.MarkFrameResult.Update;

public class StgUpdateFrame extends UpdateFrame {

    public StgUpdateFrame(Thunk updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        if (marked) {
            super.stackEnter(context);
        } else {
            updatee.updateWithIndirection(context.R(1));
        }
    }
}
