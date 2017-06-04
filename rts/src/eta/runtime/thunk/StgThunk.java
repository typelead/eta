package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.ApV;
import eta.runtime.apply.ApN;
import eta.runtime.apply.ApL;
import eta.runtime.apply.ApF;
import eta.runtime.apply.ApD;
import eta.runtime.apply.ApO;
import eta.runtime.apply.ApP;
import eta.runtime.apply.ApPV;
import eta.runtime.apply.ApPP;
import eta.runtime.apply.ApPPV;
import eta.runtime.apply.ApPPP;
import eta.runtime.apply.ApPPPV;
import eta.runtime.apply.ApPPPP;
import eta.runtime.apply.ApPPPPP;
import eta.runtime.apply.ApPPPPPP;
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

    public void thunkEnter(StgContext context) {
        barf("thunkEnter not implemented");
    }

    public final void updateWithIndirection(StgClosure ret) {
        indirectee = ret;
    }

    @Override
    public final void applyV(StgContext context) {
        if (indirectee == null) {
            context.pushFrame(new ApV());
            enter(context);
        } else {
            indirectee.applyV(context);
        }
    }

    @Override
    public void applyN(StgContext context, int n) {
        if (indirectee == null) {
            context.pushFrame(new ApN(n));
            enter(context);
        } else {
            indirectee.applyN(context, n);
        }
    }

    @Override
    public void applyL(StgContext context, long l) {
        if (indirectee == null) {
            context.pushFrame(new ApL(l));
            enter(context);
        } else {
            indirectee.applyL(context, l);
        }
    }

    @Override
    public void applyF(StgContext context, float f) {
        if (indirectee == null) {
            context.pushFrame(new ApF(f));
            enter(context);
        } else {
            indirectee.applyF(context, f);
        }
    }

    @Override
    public void applyD(StgContext context, double d) {
        if (indirectee == null) {
            context.pushFrame(new ApD(d));
            enter(context);
        } else {
            indirectee.applyD(context, d);
        }
    }

    @Override
    public void applyO(StgContext context, Object o) {
        if (indirectee == null) {
            context.pushFrame(new ApO(o));
            enter(context);
        } else {
            indirectee.applyO(context, o);
        }
    }

    @Override
    public void applyP(StgContext context, StgClosure p) {
        if (indirectee == null) {
            context.pushFrame(new ApP(p));
            enter(context);
        } else {
            indirectee.applyP(context, p);
        }
    }

    @Override
    public void applyPV(StgContext context, StgClosure p) {
        if (indirectee == null) {
            context.pushFrame(new ApPV(p));
            enter(context);
        } else {
            indirectee.applyPV(context, p);
        }
    }

    @Override
    public void applyPP(StgContext context, StgClosure p1, StgClosure p2) {
        if (indirectee == null) {
            context.pushFrame(new ApPP(p1, p2));
            enter(context);
        } else {
            indirectee.applyPP(context, p1, p2);
        }
    }

    @Override
    public void applyPPV(StgContext context, StgClosure p1, StgClosure p2) {
        if (indirectee == null) {
            context.pushFrame(new ApPPV(p1, p2));
            enter(context);
        } else {
            indirectee.applyPPV(context, p1, p2);
        }
    }

    @Override
    public void applyPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        if (indirectee == null) {
            context.pushFrame(new ApPPP(p1, p2, p3));
            enter(context);
        } else {
            indirectee.applyPPP(context, p1, p2, p3);
        }
    }

    @Override
    public void applyPPPV(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPV(p1, p2, p3));
            enter(context);
        } else {
            indirectee.applyPPPV(context, p1, p2, p3);
        }
    }

    @Override
    public void applyPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPP(p1, p2, p3, p4));
            enter(context);
        } else {
            indirectee.applyPPPP(context, p1, p2, p3, p4);
        }
    }

    @Override
    public void applyPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPPP(p1, p2, p3, p4, p5));
            enter(context);
        } else {
            indirectee.applyPPPPP(context, p1, p2, p3, p4, p5);
        }
    }

    @Override
    public void applyPPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPPPP(p1, p2, p3, p4, p5, p6));
            enter(context);
        } else {
            indirectee.applyPPPPPP(context, p1, p2, p3, p4, p5, p6);
        }
    }
}
