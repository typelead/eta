package eta.runtime.stg;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.nio.ByteBuffer;

import eta.runtime.stg.Closure;

public class StablePtrTable {

    private static StablePtrTable INSTANCE = new StablePtrTable();
    private static Object lock = new Object();

    private ArrayList<Closure> ptrs = new ArrayList<Closure>(64);
    private ArrayDeque<Integer> freeIndexes = new ArrayDeque<Integer>(20);

    private StablePtrTable() {}

    // TODO: Add a "stable ptr compactification" step to the GC
    public static int makeStablePtr(Closure p) {
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

    public static Closure getClosure(int index) {
        return INSTANCE.ptrs.get(index);
    }

    public static void free(int index) {
        synchronized (lock) {
            INSTANCE.ptrs.add(index, null);
            INSTANCE.freeIndexes.push(index);
        }
    }

    public static ByteBuffer stablePtr2Addr(int stablePtr) {
        return (ByteBuffer) ByteBuffer.allocate(4).putInt(stablePtr).rewind();
    }
}
