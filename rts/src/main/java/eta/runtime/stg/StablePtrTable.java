package eta.runtime.stg;

import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import eta.Closure;

public class StablePtrTable {

    private static StablePtrTable INSTANCE  = new StablePtrTable();
    private static Object         lock      = new Object();

    private Map<Integer, Closure> ptrs        = new ConcurrentHashMap<Integer, Closure>();
    private AtomicInteger         nextIndex   = new AtomicInteger();
    private Queue<Integer>        freeIndexes = new ConcurrentLinkedQueue<Integer>();

    private StablePtrTable() {}

    public static int makeStablePtr(Closure p) {
        return INSTANCE.createStablePtr(p);
    }

    public int createStablePtr(Closure p) {
        Integer index = freeIndexes.poll();
        if (index == null) {
            index = nextIndex.getAndIncrement();
        }
        ptrs.put(index, p);
        return index;
    }

    public static Closure getClosure(int index) {
        return INSTANCE.ptrs.get(index);
    }

    public static void free(int index) {
        Closure prev = INSTANCE.ptrs.remove(index);
        if (prev != null) {
            INSTANCE.freeIndexes.offer(index);
        }
    }
}
