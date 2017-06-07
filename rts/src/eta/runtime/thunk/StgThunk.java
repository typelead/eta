package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.util.UnsafeUtil;
import static eta.runtime.RtsMessages.barf;

public class StgThunk extends StgClosure {
    public volatile StgClosure indirectee;

    public StgThunk() {
        this(null);
    }

    public StgThunk(StgClosure indirectee) {
        super();
        this.indirectee = indirectee;
    }

    @Override
    public final StgClosure getEvaluated() {
        if (indirectee == null) return null;
        else return indirectee.getEvaluated();
    }

    @Override
    public boolean isFizzledSpark() {
        return getEvaluated() != null;
    }

    public final boolean tryLock(StgClosure oldIndirectee) {
        return cas(oldIndirectee, StgWhiteHole.closure);
    }

    public final boolean cas(StgClosure expected, StgClosure update) {
        return UnsafeUtil.cas(this, expected, update);
    }

    public StgClosure thunkEnter(StgContext context) {
        barf("thunkEnter not implemented");
    }

    public final void updateWithIndirection(StgClosure ret) {
        indirectee = ret;
    }

    @Override
    public final void applyV(StgContext context) {
        return ((indirectee == null)? enter(context):indirectee).applyV(context);
    }

    @Override
    public StgClosure applyN(StgContext context, int n) {
        return ((indirectee == null)? enter(context):indirectee).applyN(context, n);
    }

    @Override
    public StgClosure applyL(StgContext context, long l) {
        return ((indirectee == null)? enter(context):indirectee).applyL(context, l);
    }

    @Override
    public StgClosure applyF(StgContext context, float f) {
        return ((indirectee == null)? enter(context):indirectee).applyF(context, f);
    }

    @Override
    public StgClosure applyD(StgContext context, double d) {
        return ((indirectee == null)? enter(context):indirectee).applyD(context, d);
    }

    @Override
    public StgClosure applyO(StgContext context, Object o) {
        return ((indirectee == null)? enter(context):indirectee).applyO(context, o);
    }

    @Override
    public StgClosure applyP(StgContext context, StgClosure p) {
        return ((indirectee == null)? enter(context):indirectee).applyP(context, p);
    }

    @Override
    public StgClosure applyPV(StgContext context, StgClosure p) {
        return ((indirectee == null)? enter(context):indirectee).applyPV(context, p);
    }

    @Override
    public StgClosure applyPP(StgContext context, StgClosure p1, StgClosure p2) {
        return ((indirectee == null)? enter(context):indirectee).applyPP(context, p1, p2);
    }

    @Override
    public StgClosure applyPPV(StgContext context, StgClosure p1, StgClosure p2) {
        return ((indirectee == null)? enter(context):indirectee).applyPPV(context, p1, p2);
    }

    @Override
    public StgClosure applyPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        return ((indirectee == null)? enter(context):indirectee).applyPPP(context, p1, p2, p3);
    }

    @Override
    public StgClosure applyPPPV(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPV(context, p1, p2, p3);
    }

    @Override
    public StgClosure applyPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPP(context, p1, p2, p3, p4);
    }

    @Override
    public StgClosure applyPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPPP(context, p1, p2, p3, p4, p5);
    }

    @Override
    public StgClosure applyPPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        return ((indirectee == null)? enter(context):indirectee).applyPPPPPP(context, p1, p2, p3, p4, p5, p6);
    }
}
