package eta.runtime.io;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.Closure;
import eta.runtime.stg.Value;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.util.UnsafeUtil.UNSAFE;

public class MutVar extends Value {
    public volatile Closure value;

    public MutVar(Closure value) {
        this.value = value;
    }

    public void set(Closure value) {
        if (useUnsafe) {
            UNSAFE.putOrderedObject(this, mutVarOffset, value);
        } else {
            vUpdater.lazySet(this, value);
        }
    }

    /** CAS Operation Support **/
    private static final boolean useUnsafe = UnsafeUtil.UNSAFE != null;
    private static long mutVarOffset = 0;
    static {
        if (useUnsafe) {
            try {
                mutVarOffset = UNSAFE.objectFieldOffset
                    (MutVar.class.getDeclaredField("value"));
            } catch (ReflectiveOperationException e) {
                e.printStackTrace();
            }
        }
    }

    private static final AtomicReferenceFieldUpdater<MutVar, Closure> vUpdater
        = AtomicReferenceFieldUpdater.newUpdater(MutVar.class, Closure.class, "value");

    public boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return UNSAFE.compareAndSwapObject(this, mutVarOffset, expected, update);
        } else {
            return vUpdater.compareAndSet(this, expected, update);
        }
    }

    @Override
    public String toString() {
        final Closure val = value;
        return "MutVar@" + hashCode() + "[" + ((val == null)? "_" : val.toString()) + "]";
    }
}
