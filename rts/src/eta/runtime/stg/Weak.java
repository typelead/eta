package eta.runtime.stg;

import java.util.List;
import java.util.ArrayList;
import java.lang.ref.WeakReference;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import static eta.runtime.RtsMessages.barf;
import static eta.runtime.concurrent.Concurrent.SPIN_COUNT;

public final class Weak extends Value {
    // TODO: Is this the right reference type?
    public WeakReference<Closure> key;
    public WeakReference<Closure> value;
    // TODO: Should the finalizer be a weak reference as well?
    public Closure finalizer;
    public List<Closure> javaFinalizers;
    public AtomicBoolean lock = new AtomicBoolean(false);
    public boolean dead = false;

    public Weak(Closure key, Closure value, Closure finalizer) {
        this.key = new WeakReference<Closure>(key);
        this.value = new WeakReference<Closure>(value);
        this.finalizer = finalizer;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("WEAK object entered!");
        return null;
    }

    public final void lock() {
        do {
            int i = 0;
            do {
                boolean old = lock.getAndSet(true);
                if (!old) return;
            } while (++i < SPIN_COUNT);
            Thread.yield();
        } while (true);
    }

    public final void unlock() {
        lock.set(false);
    }

    public final boolean tryLock() {
        return lock.getAndSet(true);
    }

    public final void die() {
        dead = true;
    }

    public final boolean isDead() {
        return dead;
    }

    public final Closure getValue() {
        return value.get();
    }

    public final void runJavaFinalizers() {
        if (javaFinalizers != null && !javaFinalizers.isEmpty()) {
            /* TODO: Implement */
        }
    }
}
