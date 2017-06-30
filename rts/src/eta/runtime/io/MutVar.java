package eta.runtime.io;

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;

public class MutVar {
    public volatile Closure value;

    public MutVar(Closure value) {
        this.value = value;
    }

    public void set(Closure value) {
        if (useUnsafe) {
            this.value = value;
        } else {
            vUpdater.set(this, value);
        }
    }

    /** CAS Operation Support **/
    private static final boolean useUnsafe = UnsafeUtil.UNSAFE == null;
    private static final AtomicReferenceFieldUpdater<MutVar, Closure> vUpdater
        = AtomicReferenceFieldUpdater.newUpdater(MutVar.class, Closure.class, "value");

    public boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return vUpdater.compareAndSet(this, expected, update);
        } else {
            return UnsafeUtil.cas(this, expected, update);
        }
    }
}
