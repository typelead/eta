package eta.runtime.io;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RuntimeLogging.barf;

public class MutVar extends Value {
    public volatile Closure value;

    public MutVar(Closure value) {
        this.value = value;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("MutVar object entered!");
        return null;
    }

    public void set(Closure value) {
        if (useUnsafe) {
            this.value = value;
        } else {
            vUpdater.set(value);
        }
    }

    /** CAS Operation Support **/
    private static final useUnsafe = UnsafeUtil.UNSAFE == null;
    private static final AtomicReferenceFieldUpdater<MutVar, Closure> vUpdater
        = AtomicReferenceFieldUpdater.newUpdater(MutVar.class, Closure.class, "value");

    public boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return vUpdater.compareAndSet(this, expected, update);
        } else {
            return UnsafeUtil.cas(this, expectedd, update);
        }
    }
}
