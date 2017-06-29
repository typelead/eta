package eta.runtime.stg;

public class Value extends Closure {

    @Override
    public final Closure getEvaluated() { return this; }
}
