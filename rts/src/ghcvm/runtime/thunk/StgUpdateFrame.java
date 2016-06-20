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

    public StgUpdateFrame(StgInd updatee) {
        super(updatee);
    }

    @Override
    public void stackEnter(StgContext context) {
        updatee.updateWithIndirection(context.R1);
    }

    public StgMarkedUpdateFrame getMarked() {
        return new StgMarkedUpdateFrame(updatee);
    }

    @Override
    public final MarkFrameResult mark(Capability cap, StgTSO tso) {
        tso.sp.set(new StgMarkedUpdateFrame(updatee));
        StgInd bh = updatee;
        StgClosure oldIndirectee = bh.indirectee;
        retry: do {
            if (!bh.isEvaluated() && bh.indirectee != tso) {
                cap.suspendComputation(tso, this);
                tso.sp.set(new StgEnter(bh));
                return UpdateEvaluted;
            } else {
                if (RtsFlags.ModeFlags.threaded && oldIndirectee != StgWhiteHole.closure) {
                    boolean locked = bh.tryLock(oldIndirectee);
                    if (!locked) {
                        continue retry;
                    }
                }
                bh.updateWithIndirection(tso);
                return Update;
            }
        } while (true);
    }
}
