/* TODO: Taken from https://github.com/noctarius/tengi/blob/master/java/tengi-core/src/main/java/com/noctarius/tengi/core/impl/UnsafeUtil.java, replace this with proper licensing */
package ghcvm.runtime.util;

import sun.misc.Unsafe;
import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stm.StgTVar;
import ghcvm.runtime.thunk.StgThunk;
import ghcvm.runtime.io.StgMutVar;

public class UnsafeUtil {

    public static final Unsafe UNSAFE;
    private static final long indirecteeOffset;
    private static final long cvOffset;
    private static final long valueOffset;

    static {
        Unsafe unsafe;

        try {
            unsafe = findUnsafe();
        } catch (RuntimeException e) {
            unsafe = null;
        }
        if (unsafe == null) {
            throw new RuntimeException("Incompatible JVM - sun.misc.Unsafe support is missing");
        }

        try {
            indirecteeOffset = unsafe.objectFieldOffset
                (StgThunk.class.getDeclaredField("indirectee"));
            cvOffset = unsafe.objectFieldOffset
                (StgTVar.class.getDeclaredField("currentValue"));
            valueOffset = unsafe.objectFieldOffset
                (StgMutVar.class.getDeclaredField("value"));
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException();
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

    public static boolean cas(StgThunk ind, StgClosure expected, StgClosure update) {
        return UNSAFE.compareAndSwapObject(ind, indirecteeOffset, expected, update);
    }

    public static boolean cas(StgTVar tvar, StgClosure expected, StgClosure update) {
        return UNSAFE.compareAndSwapObject(tvar, cvOffset, expected, update);
    }

    public static boolean cas(StgMutVar mv, StgClosure expected, StgClosure update) {
        return UNSAFE.compareAndSwapObject(mv, valueOffset, expected, update);
    }
}
