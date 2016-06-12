package ghcvm.runtime.closure;

import ghcvm.runtime.types.Capability;
import ghcvm.runtime.types.StgTSO;

public abstract class StgClosure {
    public void enter(StgContext context) {}
    public void preEnter(StgContext context) {}
    public boolean isLocked() { return false; }
    public boolean isEvaluated() { return false; }
    public boolean isBlackHole() { return false; }
    public void thunkUpdate(Capability cap, StgTSO tso) {
        cap.checkBlockingQueues(tso);
    }
}
