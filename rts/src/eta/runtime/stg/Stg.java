package eta.runtime.stg;

import java.util.ListIterator;
import java.nio.ByteBuffer;

import eta.runtime.exception.Exception;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadFinished;

public class Stg {
    /* Weak Pointer Operations */
    public static Closure mkWeak(StgContext context, Closure key, Closure value, Closure finalizer) {
        Capability cap = context.myCapability;
        WeakPtr weak = new WeakPtr(key, value, finalizer);
        cap.weakPtrList.add(weak);
        context.O(1, weak);
        return null;
    }

    public static Closure mkWeakNoFinalizer(StgContext context, Closure key, Closure value) {
        return mkWeak(context, key, value, null);
    }

    public static Closure addJavaFinalizerToWeak(StgContext context, ByteBuffer fptr, ByteBuffer ptr, int flag, ByteBuffer eptr, WeakPtr w) {
        /* TODO: Grab finalizer args */
        w.lock();
        if (w.isDead()) {
            w.unlock();
            context.I(1, 0);
        } else {
            /* TODO: Create new finalizer */
            w.unlock();
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
            /* TODO: Create new finalizer */
            Closure finalizer = w.finalizer;
            w.die();
            w.unlock();
            w.runJavaFinalizers();
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
            w.unlock();
        }
        if (w.isDead()) {
            context.I(1, 0);
            return w;
        } else {
            context.I(1, 1);
            return w.getValue();
        }
    }

    public static Closure noDuplicate(StgContext context) {
        if (capabilities.size() > 1) {
            Capability cap = context.myCapability;
            TSO tso = context.currentTSO;
            cap.threadPaused(tso);
        }
        return null;
    }
}
