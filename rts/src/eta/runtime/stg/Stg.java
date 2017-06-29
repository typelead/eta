package eta.runtime.stg;

import java.util.ListIterator;
import java.nio.ByteBuffer;

import eta.runtime.exception.Exception;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;

public class Stg {
    /* Weak Pointer Operations */
    public static Closure mkWeak(StgContext context, Closure key, Closure value, Closure finalizer) {
        context.O(1, WeakPtr.create(key, value, finalizer));
        return null;
    }

    public static Closure mkWeakNoFinalizer(StgContext context, Closure key, Closure value) {
        return mkWeak(context, key, value, null);
    }

    public static Closure addJavaFinalizerToWeak(StgContext context, long fptr, long ptr, int flag, long eptr, WeakPtr w) {
        JavaFinalizer jfinalizer = new JavaFinalizer(flag != 0, fptr, ptr, eptr);
        if (w.isDead()) {
            context.I(1, 0);
        } else {
            w.addJavaFinalizer(jfinalizer);
            context.I(1, 1);
        }
        return null;
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

    public static Closure noDuplicate(StgContext context) {
        if (!singletonCapabilities) {
            Capability cap = context.myCapability;
            TSO tso = context.currentTSO;
            cap.threadPaused(tso);
        }
        return null;
    }
}
