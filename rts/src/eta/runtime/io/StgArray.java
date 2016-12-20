package eta.runtime.io;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public final class StgArray extends StgClosure {
    public StgClosure[] arr;

    public StgArray(StgClosure[] arr) {
        this.arr = arr;
    }

    public StgClosure get(int i) {
        return arr[i];
    }

    public void set(int i, StgClosure val) {
        arr[i] = val;
    }

    @Override
    public StgClosure getEvaluated() { return this; }

    @Override
    public void enter(StgContext context) {
        barf("StgArray object entered!");
    }

    public int size() {
        return arr.length;
    }

    public static StgArray create(int n, StgClosure init) {
        StgClosure[] arr = new StgClosure[n];
        for (int i = 0; i < n; i++) {
            arr[i] = init;
        }
        return new StgArray(arr);
    }

    public static void copyArray( StgArray srcArray, int srcOffset
                                , StgArray destArray, int destOffset, int n) {
        System.arraycopy(srcArray.arr, srcOffset, destArray.arr, destOffset, n);
    }

    public static StgArray cloneArray(StgArray srcArray, int offset, int n) {
        StgClosure[] arr = new StgClosure[n];
        System.arraycopy(srcArray.arr, offset, arr, 0, n);
        return new StgArray(arr);
    }
}
