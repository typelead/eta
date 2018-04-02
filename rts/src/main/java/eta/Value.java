package eta;

import eta.runtime.stg.StgContext;
import static eta.runtime.RuntimeLogging.barf;

public abstract class Value extends Closure {

    @Override
    public Closure enter(StgContext context) {
        barf(this + " entered.");
        return null;
    }

    @Override
    public final Closure getEvaluated() { return this; }

    @Override
    public final Closure evaluate(StgContext context) { return this; }
}
