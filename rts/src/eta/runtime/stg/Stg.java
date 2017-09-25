package eta.runtime.stg;

public class Stg {
    /* Weak Pointer Operations */
    public static WeakPtr mkWeak(StgContext context, Closure key, Closure value, Closure finalizer) {
        return WeakPtr.create(key, value, finalizer);
    }

    public static WeakPtr mkWeakNoFinalizer(StgContext context, Closure key, Closure value) {
        return mkWeak(context, key, value, null);
    }

    public static int addJavaFinalizerToWeak(StgContext context, long fptr, long ptr, int flag, long eptr, WeakPtr w) {
        JavaFinalizer jfinalizer = new JavaFinalizer(flag != 0, fptr, ptr, eptr);
        if (w.isDead()) {
            return 0;
        } else {
            w.addJavaFinalizer(jfinalizer);
            return 1;
        }
    }

    public static Closure finalizeWeak(StgContext context, WeakPtr w) {
        w.lock();
        if (w.isDead()) {
            w.unlock();
            context.I(1, 0);
            return null;
        } else {
            w.die();
            w.unlock();
            w.runJavaFinalizers();
            Closure finalizer = w.finalizer;
            if (finalizer == null) {
                context.I(1, 0);
                return null;
            } else {
                context.I(1, 1);
                return finalizer;
            }
        }
    }

    public static Closure deRefWeak(StgContext context, WeakPtr w) {
        if (!w.tryLock()) {
            w.lock();
        }
        w.unlock();
        if (w.isDead()) {
            context.I(1, 0);
            return null;
        } else {
            context.I(1, 1);
            return w.getValue();
        }
    }

    public static void noDuplicate(StgContext context) {
        if (!Capability.singletonCapabilities()) {
            Capability cap = context.myCapability;
            TSO tso = context.currentTSO;
            cap.threadPaused(tso);
        }
    }
}
