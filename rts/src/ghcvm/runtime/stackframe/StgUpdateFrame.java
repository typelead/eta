package ghcvm.runtime.stackframe;

import ghcvm.runtime.RtsFlags;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.closure.StgContext;
import ghcvm.runtime.closure.StgClosure;
import static ghcvm.runtime.thunk.StgWhiteHole.stgWhiteHole;
import static ghcvm.runtime.stackframe.StackFrame.MarkFrameResult.UpdateEvaluted;
import static ghcvm.runtime.stackframe.StackFrame.MarkFrameResult.Update;

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
                if (RtsFlags.ModeFlags.threaded && oldIndirectee != stgWhiteHole) {
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
