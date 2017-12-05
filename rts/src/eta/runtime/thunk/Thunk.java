package eta.runtime.thunk;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import java.util.Queue;
import java.util.WeakHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

import eta.runtime.stg.Value;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.util.UnsafeUtil;
import eta.runtime.message.MessageBlackHole;
import eta.runtime.exception.Exception;
import eta.runtime.exception.StgException;
import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;
import static eta.runtime.util.UnsafeUtil.UNSAFE;
import static eta.runtime.RuntimeLogging.barf;
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

    protected static boolean keepCAFs;

    public static void setKeepCAFs() {
        keepCAFs = true;
    }

    public static void resetKeepCAFs() {
        keepCAFs = false;
    }

    public static boolean shouldKeepCAFs() {
        return keepCAFs;
    }

    public static synchronized void revertCAFs() {
        for (CAF c: revertibleCAFList) {
            c.setIndirection(null);
        }
        revertibleCAFList.clear();
    }

    /* Used to facilitate the free variable clearing code and caches the field
       lookups to reduce the cost of reflection. */
    public static WeakHashMap<Class<?>, Field[]> thunkFieldsCache
      = new WeakHashMap<Class<?>, Field[]>();

    /* Clears out the free variables of a thunk using reflection to free up the
       strong references of an evaluated thunk. */
    public void clear() {
        Class<?> thisClass = getClass();
        Field[] fields = thunkFieldsCache.get(thisClass);
        int i = 0;
        if (fields == null) {
            Field[] lookupFields = thisClass.getFields();
            for (Field f:lookupFields) {
                if (canClearField(f)) {
                    i++;
                }
            }
            fields = new Field[i];
            i = 0;
            for (Field f:lookupFields) {
                if (canClearField(f)) {
                    fields[i++] = f;
                }
            }
            thunkFieldsCache.put(thisClass, fields);
        }
        for (Field f:fields) {
            try {
                f.set(this, null);
            } catch (IllegalAccessException e) {}
        }
    }

    private static boolean canClearField(Field f) {
        return !f.getName().equals("indirectee")
            && !f.getType().isPrimitive()
            && !Modifier.isStatic(f.getModifiers());
    }

    protected static boolean handleException(java.lang.Exception e, TSO tso, UpdateInfo ui) {
        if (e instanceof EtaAsyncException) {
            EtaAsyncException ea = (EtaAsyncException) e;
            if (ea.stopHere == ui) {
                return true;
            } else {
                throw ea;
            }
        } else {
            if (e instanceof StgException) {
                throw (StgException)e;
            } else {
                throw Exception.toEtaException(tso, e);
            }
        }
    }
}
