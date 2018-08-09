package eta.runtime.stg;

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

    @Override
    public final Closure evaluateTail(StgContext context) { return this; }
}
