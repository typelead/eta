package ghcvm.runtime.stg;

import java.util.List;
import java.util.ArrayList;
import java.lang.ref.WeakReference;
import java.util.concurrent.atomic.AtomicBoolean;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.RtsMessages.barf;
import static ghcvm.runtime.concurrent.Concurrent.SPIN_COUNT;

public final class StgWeak extends StgClosure {
    // TODO: Is this the right reference type?
    public WeakReference<StgClosure> key;
    public WeakReference<StgClosure> value;
    // TODO: Should the finalizer be a weak reference as well?
    public StgClosure finalizer;
    public List<StgClosure> javaFinalizers;
    public AtomicBoolean lock = new AtomicBoolean(false);
    public boolean dead = false;

    public StgWeak(StgClosure key, StgClosure value, StgClosure finalizer) {
        this.key = new WeakReference<StgClosure>(key);
        this.value = new WeakReference<StgClosure>(value);
        this.finalizer = finalizer;
    }

    @Override
    public void enter(StgContext context) {
        barf("WEAK object entered!");
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

    public final StgClosure getValue() {
        return value.get();
    }

    public final void runJavaFinalizers() {
        if (javaFinalizers != null && !javaFinalizers.isEmpty()) {
            /* TODO: Implement */
        }
    }
}
