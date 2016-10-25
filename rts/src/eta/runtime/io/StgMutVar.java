package eta.runtime.io;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RtsMessages.barf;

public class StgMutVar extends StgClosure {
    public StgClosure value;

    public StgMutVar(StgClosure value) {
        this.value = value;
    }

    @Override
    public void enter(StgContext context) {
        barf("StgMutVar object entered!");
    }

    public boolean cas(StgClosure old, StgClosure new_) {
        return UnsafeUtil.cas(this, old, new_);
    }

    @Override
    public StgClosure getEvaluated() { return this; }
}
