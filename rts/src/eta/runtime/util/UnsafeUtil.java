/* TODO: Taken from https://github.com/noctarius/tengi/blob/master/java/tengi-core/src/main/java/com/noctarius/tengi/core/impl/UnsafeUtil.java, replace this with proper licensing */
package eta.runtime.util;

import sun.misc.Unsafe;
import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;

import eta.runtime.stg.Closure;
import eta.runtime.stm.TVar;
import eta.runtime.thunk.Thunk;
import eta.runtime.io.MutVar;

public class UnsafeUtil {

    public static final Unsafe UNSAFE;
    private static long indirecteeOffset = 0;
    private static long cvOffset         = 0;
    private static long valueOffset      = 0;

    static {
        Unsafe unsafe;

        try {
            unsafe = findUnsafe();
        } catch (RuntimeException e) {
            unsafe = null;
        }
        if (unsafe != null) {
            try {
                indirecteeOffset = unsafe.objectFieldOffset
                    (Thunk.class.getDeclaredField("indirectee"));
                cvOffset = unsafe.objectFieldOffset
                    (TVar.class.getDeclaredField("currentValue"));
                valueOffset = unsafe.objectFieldOffset
                    (MutVar.class.getDeclaredField("value"));
            } catch (ReflectiveOperationException e) {
                unsafe = null;
            }
        }
        UNSAFE = unsafe;
    }

    private static Unsafe findUnsafe() {
        try {
            return Unsafe.getUnsafe();
        } catch (SecurityException se) {
            return AccessController.doPrivileged(new PrivilegedAction<Unsafe>() {
                @Override
                public Unsafe run() {
                    try {
                        Class<Unsafe> type = Unsafe.class;
                        try {
                            Field field = type.getDeclaredField("theUnsafe");
                            field.setAccessible(true);
                            return type.cast(field.get(type));

                        } catch (Exception e) {
                            for (Field field : type.getDeclaredFields()) {
                                if (type.isAssignableFrom(field.getType())) {
                                    field.setAccessible(true);
                                    return type.cast(field.get(type));
                                }
                            }
                        }
                    } catch (Exception e) {
                        throw new RuntimeException("Unsafe unavailable", e);
                    }
                    throw new RuntimeException("Unsafe unavailable");
                }
            });
        }
    }

    private UnsafeUtil() {}

    public static boolean cas(Thunk ind, Closure expected, Closure update) {
        return UNSAFE.compareAndSwapObject(ind, indirecteeOffset, expected, update);
    }

    public static boolean cas(TVar tvar, Closure expected, Closure update) {
        return UNSAFE.compareAndSwapObject(tvar, cvOffset, expected, update);
    }

    public static boolean cas(MutVar mv, Closure expected, Closure update) {
        return UNSAFE.compareAndSwapObject(mv, valueOffset, expected, update);
    }
}
