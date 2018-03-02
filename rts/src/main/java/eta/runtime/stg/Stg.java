package eta.runtime.stg;

import eta.runtime.Runtime;
import eta.runtime.thunk.*;
import eta.runtime.exception.TrampolineBounceException;

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
            context.I1 = 0;
            return null;
        } else {
            w.die();
            w.unlock();
            w.runJavaFinalizers();
            Closure finalizer = w.finalizer;
            if (finalizer == null) {
                context.I1 = 0;
                return null;
            } else {
                context.I1 = 1;
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
            context.I1 = 0;
            return null;
        } else {
            context.I1 = 1;
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

    /* Trampolining Implementation */

    public static final int TAIL_CALL_THRESHOLD = Runtime.getTailCallThreshold();

    public static final ThreadLocal<TrampolineBounceException> bounce =
        new ThreadLocal<TrampolineBounceException>() {
            @Override
            public TrampolineBounceException initialValue() {
                return new TrampolineBounceException();
            }
        };

    public static Closure trampoline(StgContext context, Closure closure) {
        int     tailCalls  = context.tailCalls;
        Closure ret        = null;
        Closure next       = closure;
        boolean trampoline = context.trampoline;
        context.trampoline = true;
        while (next != null) {
            context.tailCalls = 0;
            context.firstTime = true;
            try {
                ret  = next.enter(context);
                next = null;
            } catch (Exception e) {
                if (e instanceof TrampolineBounceException) {
                    next = context.next;
                } else {
                    context.resetTrampoline(tailCalls, trampoline);
                    throw e;
                }
            }
        }
        context.resetTrampoline(tailCalls, trampoline);
        return ret;
    }

    public static void enterTail(StgContext context, Closure node) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = node;
            throw bounce.get();
        }
    }

    // TODO: Replace Ap* with Ap*NonUpd to avoid overhead of evaluate()
    public static void applyVTail(StgContext context, Closure node) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap1VUpd(node);
            throw bounce.get();
        }
    }

    public static void applyNTail(StgContext context, Closure node, int n) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new ApI(node, n);
            throw bounce.get();
        }
    }

    public static void applyLTail(StgContext context, Closure node, long l) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new ApL(node, l);
            throw bounce.get();
        }
    }

    public static void applyFTail(StgContext context, Closure node, float f) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new ApF(node, f);
            throw bounce.get();
        }
    }

    public static void applyDTail(StgContext context, Closure node, double d) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new ApD(node, d);
            throw bounce.get();
        }
    }

    public static void applyOTail(StgContext context, Closure node, Object o) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new ApO(node, o);
            throw bounce.get();
        }
    }

    public static void apply1Tail(StgContext context, Closure node, Closure p) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap2Upd(node, p);
            throw bounce.get();
        }
    }

    public static void apply1VTail(StgContext context, Closure node, Closure p) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap2VUpd(node, p);
            throw bounce.get();
        }
    }

    public static void apply2Tail(StgContext context, Closure node, Closure p1, Closure p2) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap3Upd(node, p1, p2);
            throw bounce.get();
        }
    }

    public static void apply2VTail(StgContext context, Closure node, Closure p1, Closure p2) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap3VUpd(node, p1, p2);
            throw bounce.get();
        }
    }

    public static void apply3Tail(StgContext context, Closure node, Closure p1, Closure p2, Closure p3) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap4Upd(node, p1, p2, p3);
            throw bounce.get();
        }
    }

    public static void apply3VTail(StgContext context, Closure node, Closure p1, Closure p2, Closure p3) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap4VUpd(node, p1, p2, p3);
            throw bounce.get();
        }
    }

    public static void apply4Tail(StgContext context, Closure node, Closure p1, Closure p2, Closure p3, Closure p4) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap5Upd(node, p1, p2, p3, p4);
            throw bounce.get();
        }
    }

    public static void apply5Tail(StgContext context, Closure node, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap6Upd(node, p1, p2, p3, p4, p5);
            throw bounce.get();
        }
    }

    public static void apply6Tail(StgContext context, Closure node, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        if (++context.tailCalls >= TAIL_CALL_THRESHOLD) {
            context.next = new Ap7Upd(node, p1, p2, p3, p4, p5, p6);
            throw bounce.get();
        }
    }
}
