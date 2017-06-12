package eta.runtime.stg;

public abstract class BlackHole extends Closure {

    @Override
    public final Closure getEvaluated() { return null; }
}
