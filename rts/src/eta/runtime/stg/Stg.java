package eta.runtime.stg;

import java.util.ListIterator;
import java.nio.ByteBuffer;

import eta.runtime.exception.StgException;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;
import static eta.runtime.stg.TSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadFinished;

public class Stg {
    /* Weak pointer operations */

    public static void mkWeak(StgContext context, Closure key, Closure value, Closure finalizer) {
        Capability cap = context.myCapability;
        StgWeak weak = new StgWeak(key, value, finalizer);
        cap.weakPtrList.add(weak);
        context.O(1, weak);
    }

    public static void mkWeakNoFinalizer(StgContext context, Closure key, Closure value) {
        mkWeak(context, key, value, null);
    }

    public static void addJavaFinalizerToWeak(StgContext context, ByteBuffer fptr, ByteBuffer ptr, int flag, ByteBuffer eptr, StgWeak w) {
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
    }

    public static void finalizeWeak(StgContext context, StgWeak w) {
        w.lock();
        if (w.isDead()) {
            w.unlock();
            context.I(1, 0);
            /* TODO: Is null the correct return? */
            context.R(1, null);
        } else {
            /* TODO: Create new finalizer */
            Closure finalizer = w.finalizer;
            w.die();
            w.unlock();
            w.runJavaFinalizers();
            if (finalizer == null) {
                context.I(1, 0);
                /* TODO: Is null the correct return? */
                context.R(1, null);
            } else {
                context.I(1, 1);
                context.R(1, finalizer);
            }
        }
    }

    public static void deRefWeak(StgContext context, StgWeak w) {
        if (!w.tryLock()) {
            w.lock();
            w.unlock();
        }
        if (w.isDead()) {
            context.I(1, 0);
            context.R(1, w);
        } else {
            context.I(1, 1);
            context.R(1, w.getValue());
        }
    }

    public static void returnToSched(StgContext context) {
        Capability cap = context.myCapability;
        TSO tso = context.currentTSO;
        cap.threadPaused(tso);
        throw StgException.stgReturnException;
    }

    public static Closure noDuplicate(StgContext context) {
        if (Capability.nCapabilities != 1) {
            Capability cap = context.myCapability;
            TSO tso = context.currentTSO;
            cap.threadPaused(tso);
        }
        return null;
    }

    public static void threadFinished(StgContext context) {
        context.ret = ThreadFinished;
        throw StgException.stgReturnException;
    }
}
