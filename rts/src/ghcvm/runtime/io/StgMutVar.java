package ghcvm.runtime.io;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.util.UnsafeUtil;
import static ghcvm.runtime.RtsMessages.barf;

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
}
