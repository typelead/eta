package eta.runtime.stm;

import eta.runtime.stg.Closure;

public class TransactionEntry {
    public TVar    tvar;
    public Closure expectedValue;
    public Closure newValue;
    public int     numUpdates;

    public TransactionEntry(TVar tvar, Closure expected, Closure updated) {
        this.tvar          = tvar;
        this.expectedValue = expected;
        this.newValue      = updated;
    }

    public boolean isUpdate() {
        return expectedValue != newValue;
    }

    public boolean isReadOnly() {
        return expectedValue == newValue;
    }
}
