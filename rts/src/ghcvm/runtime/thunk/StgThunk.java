package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Void;
import ghcvm.runtime.apply.ApV;
import ghcvm.runtime.apply.ApN;
import ghcvm.runtime.apply.ApL;
import ghcvm.runtime.apply.ApF;
import ghcvm.runtime.apply.ApD;
import ghcvm.runtime.apply.ApO;
import ghcvm.runtime.apply.ApP;
import ghcvm.runtime.apply.ApPV;
import ghcvm.runtime.apply.ApPVO;
import ghcvm.runtime.apply.ApPP;
import ghcvm.runtime.apply.ApPPV;
import ghcvm.runtime.apply.ApPPVO;
import ghcvm.runtime.apply.ApPPP;
import ghcvm.runtime.apply.ApPPPV;
import ghcvm.runtime.apply.ApPPPVO;
import ghcvm.runtime.apply.ApPPPP;
import ghcvm.runtime.apply.ApPPPPP;
import ghcvm.runtime.apply.ApPPPPPP;
import static ghcvm.runtime.RtsMessages.barf;

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
