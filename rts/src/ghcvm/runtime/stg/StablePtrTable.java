package ghcvm.runtime.stg;

import java.util.ArrayList;
import java.util.ArrayDeque;

import ghcvm.runtime.stg.StgClosure;

public class StablePtrTable {

    private static StablePtrTable INSTANCE = new StablePtrTable();
    private static Object lock = new Object();

    private ArrayList<StgClosure> ptrs = new ArrayList<StgClosure>(64);
    private ArrayDeque<Integer> freeIndexes = new ArrayDeque<Integer>(20);

    private StablePtrTable() {}

    // TODO: Add a "stable ptr compactification" step to the GC
    public static int makeStablePtr(StgClosure p) {
        Integer index;
        synchronized (lock) {
            index = INSTANCE.freeIndexes.poll();
            if (index == null) {
                INSTANCE.ptrs.add(p);
                index = INSTANCE.ptrs.size() - 1;
            } else {
                INSTANCE.ptrs.add(index, p);
            }
        }
        return index.intValue();
    }

    public static StgClosure getClosure(int index) {
        return INSTANCE.ptrs.get(index);
    }

    public static void free(int index) {
        synchronized (lock) {
            INSTANCE.ptrs.add(index, null);
            INSTANCE.freeIndexes.push(index);
        }
    }
}
