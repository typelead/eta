package ghcvm.runtime.stm;

public class StgInvariantCheck {
    public StgAtomicInvariant invariant;
    public StgTRecHeader myExecution;
    public StgInvariantCheck(StgAtomicInvariant invariant) {
        this.invariant = invariant;
    }
}
