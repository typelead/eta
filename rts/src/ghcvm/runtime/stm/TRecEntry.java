package ghcvm.runtime.stm;

import ghcvm.runtime.closure.StgClosure;

public class TRecEntry {
    public StgTVar tvar;
    public StgClosure expectedValue;
    public StgClosure newValue;
    public int numUpdates;

    public boolean isUpdate() {
        return expectedValue != newValue;
    }

    public boolean isReadOnly() {
        return expectedValue == newValue;
    }
}
