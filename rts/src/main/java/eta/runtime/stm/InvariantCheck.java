package eta.runtime.stm;

public class InvariantCheck {
    public AtomicInvariant invariant;
    public TransactionRecord myExecution;
    public InvariantCheck(AtomicInvariant invariant) {
        this.invariant = invariant;
    }
}
