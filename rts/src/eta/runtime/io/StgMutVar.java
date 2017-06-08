package eta.runtime.io;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RtsMessages.barf;

public class StgMutVar extends StgValue {
    public Closure value;

    public StgMutVar(Closure value) {
        this.value = value;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("StgMutVar object entered!");
        return null;
    }

    public boolean cas(Closure old, Closure new_) {
        return UnsafeUtil.cas(this, old, new_);
    }
}
