package eta.runtime.io;

import eta.runtime.stg.Closure;
import eta.runtime.stg.Value;

public final class Array extends Value {
    public Closure[] arr;

    public Array(Closure[] arr) {
        this.arr = arr;
    }

    public Closure get(int i) {
        return arr[i];
    }

    public void set(int i, Closure val) {
        arr[i] = val;
    }

    public int size() {
        return arr.length;
    }

    /* Primitive Operation Utilities */
    public static Array create(int n, Closure init) {
        Closure[] arr = new Closure[n];
        for (int i = 0; i < n; i++) {
            arr[i] = init;
        }
        return new Array(arr);
    }

    public static void copyArray( Array srcArray, int srcOffset
                                , Array destArray, int destOffset, int n) {
        System.arraycopy(srcArray.arr, srcOffset, destArray.arr, destOffset, n);
    }

    public static Array cloneArray(Array srcArray, int offset, int n) {
        Closure[] arr = new Closure[n];
        System.arraycopy(srcArray.arr, offset, arr, 0, n);
        return new Array(arr);
    }
}
