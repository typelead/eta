package eta.runtime.thunk;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RuntimeLogging.barf;

public class Thunk extends Closure {
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

    /* Subclasses should override this. The reason it's implemented is that it's
       used for single-entry thunks. */
    public Closure thunkEnter(StgContext context) {
        barf("thunkEnter not implemented");
    }

    public final void setIndirection(Closure c) {
        if (useUnsafe) {
            indirectee = c;
        } else {
            indirectee.set(c);
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
        if (v == tso) {
            updatee.updateWithIndirection(ret);
            return ret;
        }
        updateThunk(cap, tso, ret);
        return ret;
    }

    public final void updateThunk(Capability cap, TSO tso, Closure val) {
        Closure v = indirectee;
        /* Has not been blackholed, so update with no further checks */
        if (v == null) {
            updateWithIndirection(val);
            return;
        }
        updateWithIndirection(val);
        if (v == tso) return;
        if (v instanceof BlockingQueue) {
            BlockingQueue bq = (BlockingQueue) v;
            TSO owner = bq.owner;
            if (owner != tso) {
                cap.checkBockingQueues(tso);
            } else {
                cap.wakeBlockingQueue(cap, bq);
            }
        } else {
            cap.checkBlockingQueues(tso);
            return;
        }
    }

    public final Closure blackHole(StgContext context) {
        do {
            Closure p = indirectee;
            if (p instanceof Value) return p;
            else if (p instanceof BlackHole) {
                Capability cap = context.myCapability;
                TSO tso = context.currentTSO;
                MessageBlackHole msg = new MessageBlackHole(tso, blackhole);
                boolean blocked = cap.messageBlackHole(msg);
                if (blocked) {
                    tso.whyBlocked = BlockedOnBlackHole;
                    tso.blockInfo = msg;
                    cap.idleLoop(true)
                    LockSupport.park();
                    if (Thread.interrupted()) {}
                    cap.idleLoop(true);
                }
                continue;
            } else return p.enter(context);
        } while (true);
    }

    /** Apply overrides for Thunks **/

    @Override
    public final void applyV(StgContext context) {
        return ((indirectee == null)? enter(context):indirectee).applyV(context);
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        return ((indirectee == null)? enter(context):indirectee).applyN(context, n);
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        return ((indirectee == null)? enter(context):indirectee).applyL(context, l);
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        return ((indirectee == null)? enter(context):indirectee).applyF(context, f);
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        return ((indirectee == null)? enter(context):indirectee).applyD(context, d);
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        return ((indirectee == null)? enter(context):indirectee).applyO(context, o);
    }

    @Override
    public Closure applyP(StgContext context, Closure p) {
        return ((indirectee == null)? enter(context):indirectee).applyP(context, p);
    }

    @Override
    public Closure applyPV(StgContext context, Closure p) {
        return ((indirectee == null)? enter(context):indirectee).applyPV(context, p);
    }

    @Override
    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
        return ((indirectee == null)? enter(context):indirectee).applyPP(context, p1, p2);
    }

    @Override
    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
        return ((indirectee == null)? enter(context):indirectee).applyPPV(context, p1, p2);
    }

    @Override
    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
        return ((indirectee == null)? enter(context):indirectee).applyPPP(context, p1, p2, p3);
    }

    @Override
    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPV(context, p1, p2, p3);
    }

    @Override
    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPP(context, p1, p2, p3, p4);
    }

    @Override
    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPPP(context, p1, p2, p3, p4, p5);
    }

    @Override
    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPPPP(context, p1, p2, p3, p4, p5, p6);
    }

    /** Locking Mechanism **/

    public final boolean tryLock() {
        return cas(null, StgWhiteHole.closure);
    }


    /** CAS Operation Support **/

    private static final useUnsafe = UnsafeUtil.UNSAFE == null;
    private static final AtomicReferenceFieldUpdater<Thunk, Closure> indUpdater
        = AtomicReferenceFieldUpdater
            .newUpdater(Thunk.class, Closure.class, "indirectee");

    public final boolean cas(Closure expected, Closure update) {
        if (useUnsafe) {
            return indUpdater.compareAndSet(this, expected, update);
        } else {
            return UnsafeUtil.cas(this, expected, update);
        }
    }

    /** Keep CAFs

       This allows clients to reset all CAFs to unevaluated state.
    **/
    private static Queue<CAF> revertibleCAFList = new ConcurrentLinkedQueue<CAF>();

    private static boolean keepCAFs;

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
                if (!f.getName().equals("indirectee") && !f.getType().isPrimitive()) {
                    i++;
                }
            }
            fields = new Field[i];
            i = 0;
            for (Field f:lookupFields) {
                if (!f.getName().equals("indirectee") && !f.getType().isPrimitive()) {
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
}
