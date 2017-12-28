package eta.runtime.stg;

import java.lang.reflect.Method;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class FunPtr {
    private static Map<Long, Method> funPtrMap =
        new ConcurrentHashMap<Long, Method>();

    private static AtomicLong funPtrId = new AtomicLong(1);

    public static long registerFunPtr(Method m) {
        long id = funPtrId.getAndIncrement();
        funPtrMap.put(id, m);
        return id;
    }

    public static Method get(long funPtrId) {
        return funPtrMap.get(funPtrId);
    }
}
