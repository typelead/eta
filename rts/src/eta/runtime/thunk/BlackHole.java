package eta.runtime.thunk;

import eta.runtime.stg.Closure;

public abstract class BlackHole extends Closure {

    @Override
    public final Closure getEvaluated() { return null; }
}
