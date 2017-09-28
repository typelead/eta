package eta.runtime.concurrent;

import eta.runtime.exception.FiberYieldException;

public class Fiber {
    public static final ThreadLocal<FiberYieldException> yieldException =
        new ThreadLocal<FiberYieldException>() {
            @Override
            public FiberYieldException initialValue() {
                return new FiberYieldException();
            }
        };
}
