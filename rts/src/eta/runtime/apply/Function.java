package eta.runtime.apply;

import eta.runtime.stg.Value;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.AbstractArgumentStack;

public abstract class Function extends Value {

    public abstract int arity();

    @Override
    public Closure applyV(StgContext context) {
        int arity = arity();
        if (arity == 1) {
            return enter(context);
        } else {
            return new PAP(arity - 1, this);
        }
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        int arity = arity();
        if (arity == 1) {
            context.I(1, n);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .add(n)
                              .build());
        }
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        int arity = arity();
        if (arity == 1) {
            context.L(1, l);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .add(l)
                              .build());
        }
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        int arity = arity();
        if (arity == 1) {
            context.F(1, f);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .add(f)
                              .build());
        }
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        int arity = arity();
        if (arity == 1) {
            context.D(1, d);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .add(d)
                              .build());
        }
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        int arity = arity();
        if (arity == 1) {
            context.O(1, o);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .add(o)
                              .build());
        }
    }

    @Override
    public Closure applyP(StgContext context, Closure p) {
        int arity = arity();
        if (arity == 1) {
            context.R(2, p);
            return enter(context);
        } else {
            return new PAP(arity - 1, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .addC(p)
                              .build());
        }
    }

    @Override
    public Closure applyPV(StgContext context, Closure p) {
        int arity = arity();
        if (arity == 1) {
            context.R(2, p);
            return enter(context).applyV(context);
        } else if (arity == 2) {
            context.R(2, p);
            return enter(context);
        } else {
            return new PAP(arity - 2, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .addC(p)
                              .build());
        }
    }

    @Override
    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
        int arity = arity();
        if (arity == 1) {
            context.R(2, p1);
            return enter(context).applyP(context, p2);
        } else if (arity == 2) {
            context.R(2, p1);
            context.R(3, p2);
            return enter(context);
        } else {
            return new PAP(arity - 2, this,
                              AbstractArgumentStack.Builder
                              .from(null)
                              .addC(p1)
                              .addC(p2)
                              .build());
        }
    }

    @Override
    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPV(context, p2);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyV(context);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context);
            default:
                return new PAP(arity - 3, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .build());
        }
    }

    @Override
    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPP(context, p2, p3);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyP(context, p3);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context);
            default:
                return new PAP(arity - 3, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .addC(p3)
                                  .build());
        }
    }

    @Override
    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPPV(context, p2, p3);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyPV(context, p3);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context).applyV(context);
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context);
            default:
                return new PAP(arity - 4, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .addC(p3)
                                  .build());
        }
    }

    @Override
    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPPP(context, p2, p3, p4);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyPP(context, p3, p4);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context).applyP(context, p4);
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                return enter(context);
            default:
                return new PAP(arity - 4, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .addC(p3)
                                  .addC(p4)
                                  .build());
        }
    }

    @Override
    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPPPP(context, p2, p3, p4, p5);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyPPP(context, p3, p4, p5);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context).applyPP(context, p4, p5);
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                return enter(context).applyP(context, p5);
            case 5:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                return enter(context);
            default:
                return new PAP(arity - 5, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .addC(p3)
                                  .addC(p4)
                                  .addC(p5)
                                  .build());
        }
    }

    @Override
    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        int arity = arity();
        switch (arity) {
            case 1:
                context.R(2, p1);
                return enter(context).applyPPPPP(context, p2, p3, p4, p5, p6);
            case 2:
                context.R(2, p1);
                context.R(3, p2);
                return enter(context).applyPPPP(context, p3, p4, p5, p6);
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                return enter(context).applyPPP(context, p4, p5, p6);
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                return enter(context).applyPP(context, p5, p6);
            case 5:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                return enter(context).applyP(context, p6);
            case 6:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                context.R(7, p6);
                return enter(context);
            default:
                return new PAP(arity - 6, this,
                                  AbstractArgumentStack.Builder
                                  .from(null)
                                  .addC(p1)
                                  .addC(p2)
                                  .addC(p3)
                                  .addC(p4)
                                  .addC(p5)
                                  .addC(p6)
                                  .build());
        }
    }
}
