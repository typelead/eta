package eta.runtime.io;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;

public final class StgArray extends Value {
    public Closure[] arr;

    public StgArray(Closure[] arr) {
        this.arr = arr;
    }

    public Closure get(int i) {
        return arr[i];
    }

    public void set(int i, Closure val) {
        arr[i] = val;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("StgArray object entered!");
        return null;
    }

    public int size() {
        return arr.length;
    }

    public static StgArray create(int n, Closure init) {
        Closure[] arr = new Closure[n];
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
        Closure[] arr = new Closure[n];
        System.arraycopy(srcArray.arr, offset, arr, 0, n);
        return new StgArray(arr);
    }
}
