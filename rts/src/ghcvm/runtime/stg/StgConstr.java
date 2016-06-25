package ghcvm.runtime.stg;

public abstract class StgConstr extends StgClosure {
    public abstract int getTag();

    @Override
    public StgClosure getEvaluated() { return this; }
}
