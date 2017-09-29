package eta.runtime.concurrent;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Value;
import eta.runtime.util.UnsafeUtil;

public class MVar extends Value {
    public volatile Closure value;
    public Queue<TSO> listeners = new ConcurrentLinkedQueue<TSO>();

    public MVar(Closure value) {
        this.value = value;
    }

    public Closure tryTake() {
        Closure val = value;
        if (val != null && cas(val, null)) {
            return val;
        }
        return null;
    }

    public boolean tryPut(Closure closure) {
        return cas(null, closure);
    }

    public Closure tryRead() {
        return value;
    }

    public void addListener(TSO tso) {
        listeners.offer(tso);
    }

    public TSO grabListener() {
        return listeners.poll();
    }

    private static final boolean useUnsafe = UnsafeUtil.UNSAFE == null;
    private static final AtomicReferenceFieldUpdater<MVar, Closure> valueUpdater
        = AtomicReferenceFieldUpdater
        .newUpdater(MVar.class, Closure.class, "value");

    public final boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return valueUpdater.compareAndSet(this, expected, update);
        } else {
            return UnsafeUtil.cas(this, expected, update);
        }
    }
}
