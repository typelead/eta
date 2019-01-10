package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

import static eta.runtime.RuntimeLogging.barf;

public abstract class BlackHole extends Closure {

    @Override
    public Closure enter(StgContext context) {
        barf("BlackHole object " + this + " entered.");
        return null;
    }
}
