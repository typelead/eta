package eta.runtime.stm;

import java.util.Deque;
import java.util.ArrayDeque;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil.cas
import static eta.runtime.RtsMessages.barf;

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

    @Override
    public final Closure enter(StgContext context) {
        barf("TVAR object entered!");
        return null;
    }

    /** Watch Queue **/

    public void removeFromWatchQueue(TSO tso) {
        watchQueue.remove(tso);
    }

    public void offerWatchQueue(TSO tso) {
        watchQueue.add(tso);
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
        } while (!cas(this, result, trec));
        return result;
    }

    public boolean conditionalLock(TransactionRecord trec, Closure expected) {
        return cas(this, expected, trec);
    }

    public void unlock(Closure c) {
        currentValue = c;
    }

    public void isLocked(TransactionRecord trec) {
        return currentValue == trec;
    }

    /** CAS Operation Support **/

    private static final AtomicReferenceFieldUpdater<TVar, Closure> cvUpdater
        = AtomicReferenceFieldUpdater
            .newUpdater(TVar.class, Closure.class, "currentValue");

    public static boolean cas(TVar tvar, Closure expected, Closure update) {
        if (UnsafeUtil.UNSAFE == null) {
            return cvUpdater.compareAndSet(tvar, expected, update);
        } else {
            return UnsafeUtil.cas(tvar, expected, update);
        }
    }
}
