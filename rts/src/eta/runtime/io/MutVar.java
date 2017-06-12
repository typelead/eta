package eta.runtime.io;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RuntimeLogging.barf;

public class MutVar extends Value {
    public Closure value;

    public MutVar(Closure value) {
        this.value = value;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("MutVar object entered!");
        return null;
    }

    public boolean cas(Closure old, Closure new_) {
        return UnsafeUtil.cas(this, old, new_);
    }
}
