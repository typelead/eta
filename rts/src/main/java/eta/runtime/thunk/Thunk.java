package eta.runtime.thunk;

import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.Runtime;
import eta.runtime.stg.Value;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.util.UnsafeUtil;
import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;
import eta.runtime.exception.TrampolineBounceException;
import eta.runtime.exception.Raise;
import eta.runtime.exception.StgException;
import static eta.runtime.util.UnsafeUtil.UNSAFE;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public abstract class Thunk extends Closure {
    public volatile Closure indirectee;

    public Thunk() {
        this(null);
    }

    public Thunk(Closure indirectee) {
        super();
        this.indirectee = indirectee;
    }

    @Override
    public final Closure getEvaluated() {
        if (indirectee instanceof Value) return indirectee;
        else return null;
    }

    @Override
    public final Closure enter(StgContext context) {
        return evaluate(context);
    }

    public abstract Closure thunkEnter(StgContext context);

    public final void setIndirection(Closure c) {
        if (useUnsafe) {
            indirectee = c;
        } else {
            indUpdater.set(this, c);
        }
    }

    public final void updateWithIndirection(Closure ret) {
        setIndirection(ret);
        clear();
    }

    public final Closure updateCode(StgContext context, Closure ret) {
        Closure v = indirectee;
        Capability cap = context.myCapability;
        TSO tso = context.currentTSO;
        if (v instanceof Value) {
            cap.checkBlockingQueues(tso);
            return v;
        }
        updateWithIndirection(ret);
        if (v != null && v != tso) {
            updateThunk(cap, tso, v);
        }
        return ret;
    }

    public final void updateThunk(Capability cap, TSO tso, Closure v) {
        if (v instanceof BlockingQueue) {
            BlockingQueue bq = (BlockingQueue) v;
            if (bq.owner == tso) {
                cap.wakeBlockingQueue(bq);
                tso.blockingQueues.remove(bq);
                return;
            }
        }
        cap.checkBlockingQueues(tso);
    }

    public final Closure blackHole(StgContext context) {
        for (;;) {
            Closure p = indirectee;
            if (p instanceof Value) return p;
            else if (p instanceof BlackHole) {
                handleBlackHole(context);
                continue;
            } else return p.enter(context);
        }
    }

    public final void handleBlackHole(StgContext context) {
        TSO tso         = context.currentTSO;
        Capability cap  = context.myCapability;
        if (cap.messageBlackHole(this, tso, false)) {
            if (tso.whyBlocked != BlockedOnBlackHole) {
                tso.whyBlocked = BlockedOnBlackHole;
                tso.blockInfo  = this;
            }
            cap.blockedLoop();
        }
    }

    /** Apply overrides for Thunks **/

    @Override
    public Closure applyV(StgContext context) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyV(context);
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyN(context, n);
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyL(context, l);
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyF(context, f);
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyD(context, d);
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).applyO(context, o);
    }

    @Override
    public Closure apply1(StgContext context, Closure p) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply1(context, p);
    }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply1V(context, p);
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply2(context, p1, p2);
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply2V(context, p1, p2);
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply3(context, p1, p2, p3);
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply3V(context, p1, p2, p3);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply4(context, p1, p2, p3, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        return ((indirectee instanceof Value)? indirectee:evaluate(context)).apply5(context, p1, p2, p3, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        return ((indirectee != null)? indirectee:evaluate(context)).apply6(context, p1, p2, p3, p4, p5, p6);
    }

    /** Locking Mechanism **/

    public final boolean tryLock() {
        return cas(null, WhiteHole.closure);
    }

    /** CAS Operation Support **/

    private static final boolean useUnsafe = UnsafeUtil.UNSAFE != null;
    private static long indirecteeOffset = 0;
    static {
        if (useUnsafe) {
            try {
                indirecteeOffset = UNSAFE.objectFieldOffset
                    (Thunk.class.getDeclaredField("indirectee"));
            } catch (ReflectiveOperationException e) {
                e.printStackTrace();
            }
        }
    }

    private static final AtomicReferenceFieldUpdater<Thunk, Closure> indUpdater
        = AtomicReferenceFieldUpdater
            .newUpdater(Thunk.class, Closure.class, "indirectee");

    public final boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return UNSAFE.compareAndSwapObject(this, indirecteeOffset, expected, update);
        } else {
            return indUpdater.compareAndSet(this, expected, update);
        }
    }

    /** Keep CAFs

       This allows clients to reset all CAFs to unevaluated state.
    **/
    protected static Queue<CAF> revertibleCAFList = new ConcurrentLinkedQueue<CAF>();

    public static void setKeepCAFs() {
        Runtime.setKeepCAFs(true);
    }

    public static void resetKeepCAFs() {
        Runtime.setKeepCAFs(false);
    }

    public static boolean shouldKeepCAFs() {
        return Runtime.shouldKeepCAFs();
    }

    public static synchronized void revertCAFs() {
        for (CAF c: revertibleCAFList) {
            c.setIndirection(null);
        }
        revertibleCAFList.clear();
    }

    /* Clears out the free variables of a thunk. The code generator will generate the clearing
       code for all the thunks. */
    public void clear() {}

    protected boolean handleException(StgContext context, java.lang.Exception e) {
        StgException thrw = handleExceptionSimple(context, e);
        if (thrw == null) return true;
        throw thrw;
    }

    public StgException handleExceptionSimple(StgContext context, java.lang.Exception e) {
        StgException thrw = null;
        if (e instanceof EtaAsyncException) {
            EtaAsyncException ea = (EtaAsyncException) e;
            if (ea.stopHere == this) {
                return null;
            } else {
                thrw = ea;
            }
        } else {
            if (e instanceof StgException) {
                thrw = (StgException)e;
            } else {
                thrw = EtaException.fromJavaException(context.currentTSO, e);
            }
        }
        if (thrw instanceof EtaException) {
            Closure raise = context.raise;
            if (raise == null) {
                context.raise = raise = new Raise(((EtaException) thrw).exception);
            }
            updateCode(context, raise);
        } else if (thrw instanceof TrampolineBounceException) {
            context.addPendingThunk(this);
        }
        //TODO: Handle EtaAsyncExceptions?
        return thrw;
    }
}
