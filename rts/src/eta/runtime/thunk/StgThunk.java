package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RtsMessages.barf;

public class StgThunk extends Closure {
    public volatile Closure indirectee;

    public StgThunk() {
        this(null);
    }

    public StgThunk(Closure indirectee) {
        super();
        this.indirectee = indirectee;
    }

    @Override
    public final Closure getEvaluated() {
        if (indirectee instanceof StgValue) return indirectee;
        else return null;
    }

    @Override
    public boolean isFizzledSpark() {
        return getEvaluated() != null;
    }

    public final boolean tryLock(Closure oldIndirectee) {
        return cas(oldIndirectee, StgWhiteHole.closure);
    }

    public final boolean cas(Closure expected, Closure update) {
        return UnsafeUtil.cas(this, expected, update);
    }

    public Closure thunkEnter(StgContext context) {
        barf("thunkEnter not implemented");
    }

    public final void updateWithIndirection(Closure ret) {
        indirectee = ret;
    }

    public final Closure updateCode(StgContext context, Closure ret) {
        Closure v = indirectee;
        Capability cap = context.myCapability;
        StgTSO tso = context.currentTSO;
        if (v instanceof StgValue) {
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

    public final void updateThunk(Capability cap, StgTSO tso, Closure val) {
        Closure v = indirectee;
        /* Has not been blackholed, so update with no further checks */
        if (v == null) {
            updateWithIndirection(val);
            return;
        }
        updateWithIndirection(val);
        if (v == tso) return;
        if (v instanceof StgBlockingQueue) {
            StgBlockingQueue bq = (StgBlockingQueue) v;
            StgTSO owner = bq.owner;
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
            if (p instanceof StgValue) return p;
            else if (p instanceof StgEvaluating) {
                Capability cap = context.myCapability;
                StgTSO tso = context.currentTSO;
                MessageBlackHole msg = new MessageBlackHole(tso, blackhole, Thread.currentThread());
                boolean blocked = cap.messageBlackHole(msg);
                if (blocked) {
                    tso.whyBlocked = BlockedOnBlackHole;
                    tso.blockInfo = msg;
                    /* TODO: Best spot to do some checks here, say for
                             asynchronous exceptions. */
                    LockSupport.park();
                    /* Takes into account that LockSupport.park() can wake up
                       spuriously. */
                    tso.whyBlocked = NotBlocked;
                    tso.blockInfo = null;
                }
                continue;
            } else {
                p.enter(context);
            }
        } while (false);
    }

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
}
