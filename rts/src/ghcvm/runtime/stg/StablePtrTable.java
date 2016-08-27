package ghcvm.runtime.stg;

import java.util.ArrayList;

import ghcvm.runtime.stg.StgClosure;

public class StablePtrTable {

    private static StablePtrTable INSTANCE = new StablePtrTable();

    private ArrayList<StgClosure> ptrs = new ArrayList<StgClosure>(64);

    private StablePtrTable() {}

    // TODO: Add a "stable ptr compactification" step to the GC
    public static synchronized int makeStablePtr(StgClosure p) {
        INSTANCE.ptrs.add(p);
        return INSTANCE.ptrs.size() - 1;
    }

    public static StgClosure getClosure(int index) {
        return INSTANCE.ptrs.get(index);
    }
}
