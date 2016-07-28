package ghcvm.runtime.io;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.RtsMessages.barf;

public class StgArray extends StgClosure {
    private StgClosure[] arr;

    public StgArray(int n, StgClosure init) {
        // TODO: Perform initialization else where?
        arr = new StgClosure[n];
        for (int i = 0; i < n; i++) {
            arr[i] = init;
        }
    }

    public StgClosure get(int i) {
        return arr[i];
    }

    public void set(int i, StgClosure val) {
        arr[i] = val;
    }

    @Override
    public void enter(StgContext context) {
        barf("StgArray object entered!");
    }

    public int size() {
        return arr.length;
    }
}
