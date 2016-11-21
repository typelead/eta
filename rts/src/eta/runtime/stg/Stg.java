package eta.runtime.stg;

import java.util.ListIterator;

import eta.runtime.exception.StgException;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadRunGHC;
import static eta.runtime.stg.StgTSO.WhatNext.ThreadKilled;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadFinished;

public class Stg {
    /* Weak pointer operations */
    public static RtsFun mkWeak = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgClosure key = context.R(1);
                StgClosure value = context.R(2);
                StgClosure finalizer = context.R(3);
                StgWeak weak = new StgWeak(key, value, finalizer);
                cap.weakPtrList.add(weak);
                context.O(1, weak);
            }
        };

    public static RtsFun mkWeakNoFinalizzer = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                context.R(3, null);
                mkWeak.enter(context);
            }
        };

    public static RtsFun addJavaFinalizzerToWeak = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                /* TODO: Grab finalizer args */
                StgWeak w = (StgWeak) context.O(1);
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
        };

    public static RtsFun finalizzeWeak = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgWeak w = (StgWeak) context.O(1);
                w.lock();
                if (w.isDead()) {
                    w.unlock();
                    context.I(1, 0);
                    /* TODO: Is null the correct return? */
                    context.R(1, null);
                } else {
                    /* TODO: Create new finalizer */
                    StgClosure finalizer = w.finalizer;
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
        };

    public static RtsFun deRefWeak = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgWeak w = (StgWeak) context.O(1);
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
        };

    public static RtsFun returnToSched = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                cap.threadPaused(tso);
                throw StgException.stgReturnException;
            }
        };

    public static RtsFun returnToSchedButFirst = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                cap.threadPaused(tso);
                StgClosure first = context.R(2);
                first.enter(context);
            }
        };

    public static RtsFun noDuplicate = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                if (Capability.nCapabilities != 1) {
                    Capability cap = context.myCapability;
                    StgTSO tso = context.currentTSO;
                    ListIterator<StackFrame> sp = tso.sp;
                    sp.add(new NoDuplicateFrame());
                    cap.threadPaused(tso);
                    if (tso.whatNext == ThreadKilled) {
                        threadFinished.enter(context);
                    } else {
                        StackFrame top = tso.stack.peek();
                        if (top.getClass() == NoDuplicateFrame.class) {
                            sp.previous();
                            sp.remove();
                        }
                        throw StgException.stackReloadException;
                    }
                }
            }
        };

    public static RtsFun threadFinished = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                context.ret = ThreadFinished;
                throw StgException.stgReturnException;
            }
        };

    public static RtsFun block_noregs = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                returnToSched.enter(context);
            }
        };
}
