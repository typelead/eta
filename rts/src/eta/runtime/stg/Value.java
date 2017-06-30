package eta.runtime.stg;

public abstract class Value extends Closure {

    @Override
    public final Closure getEvaluated() { return this; }
}
