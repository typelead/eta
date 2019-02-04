package eta.runtime.stm;

import java.util.Set;
import java.util.LinkedHashSet;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Value;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.util.UnsafeUtil.UNSAFE;

public class TVar extends Value {
    public volatile Closure currentValue;
    public Set<TSO> watchQueue = new LinkedHashSet<TSO>();
    public Set<AtomicInvariant> invariants = new LinkedHashSet<AtomicInvariant>();
    public int numUpdates;

    public TVar(Closure currentValue) {
        this.currentValue = currentValue;
    }

    public Closure currentValue() {
        Closure result;
        do {
            result = currentValue;
        } while (result instanceof TransactionRecord);
        return result;
    }

    /** Watch Queue **/

    public void removeFromWatchQueue(TSO tso) {
        watchQueue.remove(tso);
    }

    public void offerWatchQueue(TSO tso) {
        watchQueue.add(tso);
    }

    public void unparkWaiters(Capability c) {
        for (TSO tso: watchQueue) {
            tso.unpark(c);
        }
    }

    /** Invariants **/

    public Set<AtomicInvariant> getInvariants() {
        return invariants;
    }

    public void addInvariant(AtomicInvariant inv) {
        invariants.add(inv);
    }

    public void removeInvariant(AtomicInvariant inv) {
        invariants.remove(inv);
    }

    /** Locking Mechanisms **/

    public Closure lock(TransactionRecord trec) {
        Closure result;
        do {
            result = currentValue();
        } while (!cas(result, trec));
        return result;
    }

    public boolean conditionalLock(TransactionRecord trec, Closure expected) {
        return cas(expected, trec);
    }

    public void unlock(Closure c) {
        if (useUnsafe) {
            currentValue = c;
        } else {
            cvUpdater.set(this, c);
        }
    }

    public boolean isLocked(TransactionRecord trec) {
        return currentValue == trec;
    }

    /** CAS Operation Support **/

    private static final boolean useUnsafe = UnsafeUtil.UNSAFE != null;
    private static long tVarOffset = 0;
    static {
        if (useUnsafe) {
            try {
                tVarOffset = UNSAFE.objectFieldOffset
                    (TVar.class.getDeclaredField("currentValue"));
            } catch (ReflectiveOperationException e) {
                e.printStackTrace();
            }
        }
    }

    private static final AtomicReferenceFieldUpdater<TVar, Closure> cvUpdater
        = AtomicReferenceFieldUpdater
            .newUpdater(TVar.class, Closure.class, "currentValue");

    public final boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return UNSAFE.compareAndSwapObject(this, tVarOffset, expected, update);
        } else {
            return cvUpdater.compareAndSet(this, expected, update);
        }
    }
}
