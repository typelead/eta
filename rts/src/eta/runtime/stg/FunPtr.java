package eta.runtime.stg;

public class FunPtr {
    private static ConcurrentMap<Long, Method> funPtrMap =
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
