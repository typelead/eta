package eta.runtime.stg;

public class StgValue extends Closure {

    @Override
    public final Closure getEvaluated() { return this; }
}
