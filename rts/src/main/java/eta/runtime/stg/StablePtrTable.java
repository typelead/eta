package eta.runtime.stg;

import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

import eta.runtime.Runtime;
import static eta.runtime.RuntimeLogging.*;

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
        if (Runtime.debugStablePtr()) {
            debugStablePtr("stablePtrTable: PUT " + index + " " + System.identityHashCode(p));
        }
        return index;
    }

    public static Closure getClosure(int index) {
        Closure result = INSTANCE.ptrs.get(index);
        if (Runtime.debugStablePtr()) {
            debugStablePtr("stablePtrTable: GET " + index + " " + System.identityHashCode(result));
        }
        return result;

    }

    public static void free(int index) {
        Closure prev = INSTANCE.ptrs.remove(index);
        if (Runtime.debugStablePtr()) {
            debugStablePtr("stablePtrTable: ATTEMPTING TO FREE " + index + " " + System.identityHashCode(prev));
        }
        if (prev != null) {
            if (Runtime.debugStablePtr()) {
                debugStablePtr("stablePtrTable: FREE " + index + " " + System.identityHashCode(prev));
            }
            INSTANCE.freeIndexes.offer(index);
        }
    }
}
