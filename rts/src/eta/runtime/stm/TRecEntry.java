package eta.runtime.stm;

import eta.runtime.stg.Closure;

public class TRecEntry {
    public StgTVar tvar;
    public Closure expectedValue;
    public Closure newValue;
    public int numUpdates;

    public boolean isUpdate() {
        return expectedValue != newValue;
    }

    public boolean isReadOnly() {
        return expectedValue == newValue;
    }
}
