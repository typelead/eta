package eta.runtime.thunk;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.apply.Void;
import eta.runtime.apply.ApV;
import eta.runtime.apply.ApN;
import eta.runtime.apply.ApL;
import eta.runtime.apply.ApF;
import eta.runtime.apply.ApD;
import eta.runtime.apply.ApO;
import eta.runtime.apply.ApP;
import eta.runtime.apply.ApPV;
import eta.runtime.apply.ApPVO;
import eta.runtime.apply.ApPP;
import eta.runtime.apply.ApPPV;
import eta.runtime.apply.ApPPVO;
import eta.runtime.apply.ApPPP;
import eta.runtime.apply.ApPPPV;
import eta.runtime.apply.ApPPPVO;
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
    public final void apply(StgContext context, Void v) {
        if (indirectee == null) {
            context.pushFrame(new ApV());
            enter(context);
        } else {
            indirectee.apply(context, v);
        }
    }

    @Override
    public void apply(StgContext context, int n) {
        if (indirectee == null) {
            context.pushFrame(new ApN(n));
            enter(context);
        } else {
            indirectee.apply(context, n);
        }
    }

    @Override
    public void apply(StgContext context, long l) {
        if (indirectee == null) {
            context.pushFrame(new ApL(l));
            enter(context);
        } else {
            indirectee.apply(context, l);
        }
    }

    @Override
    public void apply(StgContext context, float f) {
        if (indirectee == null) {
            context.pushFrame(new ApF(f));
            enter(context);
        } else {
            indirectee.apply(context, f);
        }
    }

    @Override
    public void apply(StgContext context, double d) {
        if (indirectee == null) {
            context.pushFrame(new ApD(d));
            enter(context);
        } else {
            indirectee.apply(context, d);
        }
    }

    @Override
    public void apply(StgContext context, Object o) {
        if (indirectee == null) {
            context.pushFrame(new ApO(o));
            enter(context);
        } else {
            indirectee.apply(context, o);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p) {
        if (indirectee == null) {
            context.pushFrame(new ApP(p));
            enter(context);
        } else {
            indirectee.apply(context, p);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v) {
        if (indirectee == null) {
            context.pushFrame(new ApPV(p));
            enter(context);
        } else {
            indirectee.apply(context, p, v);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v, Object o) {
        if (indirectee == null) {
            context.pushFrame(new ApPVO(p, o));
            enter(context);
        } else {
            indirectee.apply(context, p, v, o);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2) {
        if (indirectee == null) {
            context.pushFrame(new ApPP(p1, p2));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v) {
        if (indirectee == null) {
            context.pushFrame(new ApPPV(p1, p2));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, v);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v, Object o) {
        if (indirectee == null) {
            context.pushFrame(new ApPPVO(p1, p2, o));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, v, o);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        if (indirectee == null) {
            context.pushFrame(new ApPPP(p1, p2, p3));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, Void v) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPV(p1, p2, p3));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3, v);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, Void v, Object o) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPVO(p1, p2, p3, o));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3, v, o);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPP(p1, p2, p3, p4));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3, p4);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPPP(p1, p2, p3, p4, p5));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3, p4, p5);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        if (indirectee == null) {
            context.pushFrame(new ApPPPPPP(p1, p2, p3, p4, p5, p6));
            enter(context);
        } else {
            indirectee.apply(context, p1, p2, p3, p4, p5, p6);
        }
    }
}
