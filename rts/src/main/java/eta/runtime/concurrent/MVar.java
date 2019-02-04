package eta.runtime.concurrent;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Value;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.util.UnsafeUtil.UNSAFE;

public class MVar extends Value {
    public volatile Closure value;
    public volatile TSO top;

    public MVar(Closure value) {
        this.value = value;
    }

    @Override
    public String toString() {
        final Closure val = value;
        return "MVar@" + hashCode() + "[" + ((val == null)? "_" : val.toString()) + "]";
    }

    public Closure tryTake() {
        Closure val = value;
        if (val != null && casValue(val, null)) {
            return val;
        }
        return null;
    }

    public boolean tryPut(Closure closure) {
        if (value == null && casValue(null, closure)) {
            return true;
        }
        return false;
    }

    public Closure tryRead() {
        return value;
    }

    public final void registerListener(TSO tso) {
        tso.link = TSO.TRANSIENT_LINK;
        for (;;) {
            TSO oldTop = top;
            if (casTop(oldTop, tso)) {
                /* TRANSIENT_LINK forms a synchronization point.
                   When this cas succeeds, there's a narrow window
                   where the cas from the getListeners() can happen
                   and the client can try to traverse the listeners
                   queue. Thus, a client is expected to check for link
                   values of TRANSIENT_LINK before terminating a traversal. */
                tso.link = oldTop;
                return;
            }
        }
    }

    public final TSO getListeners() {
        for (;;) {
            TSO oldTop = top;
            if (oldTop == null) {
                return null;
            } else if (casTop(oldTop, null)) {
                return oldTop;
            }
        }
    }

    private static final boolean useUnsafe = UnsafeUtil.UNSAFE != null;
    private static long mvarValueOffset    = 0;
    private static long mvarTopOffset      = 0;

    static {
        if (useUnsafe) {
            try {
                mvarValueOffset = UNSAFE.objectFieldOffset
                    (MVar.class.getDeclaredField("value"));
                mvarTopOffset = UNSAFE.objectFieldOffset
                    (MVar.class.getDeclaredField("top"));
            } catch (ReflectiveOperationException e) {
                e.printStackTrace();
            }
        }
    }

    private static final AtomicReferenceFieldUpdater<MVar, Closure> valueUpdater
        = AtomicReferenceFieldUpdater
        .newUpdater(MVar.class, Closure.class, "value");
    private static final AtomicReferenceFieldUpdater<MVar, TSO> topUpdater
        = AtomicReferenceFieldUpdater
        .newUpdater(MVar.class, TSO.class, "top");

    public final boolean casValue(Closure expected, Closure update) {
        if (useUnsafe) {
            return UNSAFE.compareAndSwapObject(this, mvarValueOffset, expected, update);
        } else {
            return valueUpdater.compareAndSet(this, expected, update);
        }
    }

    public final boolean casTop(TSO expected, TSO update) {
        if (useUnsafe) {
            return UNSAFE.compareAndSwapObject(this, mvarTopOffset, expected, update);
        } else {
            return topUpdater.compareAndSet(this, expected, update);
        }
    }
}
