package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.AbstractArgumentStack;
import ghcvm.runtime.stg.SimpleArgumentStack;

public abstract class StgFun extends StgClosure {

    public abstract int getArity();

    @Override
    public StgClosure getEvaluated() { return this; }

    @Override
    public void apply(StgContext context, Void v) {
        int arity = getArity();
        if (arity == 1) {
            enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, this);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, int n) {
        int arity = getArity();
        if (arity == 1) {
            context.I(1, n);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .add(n)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, long l) {
        int arity = getArity();
        if (arity == 1) {
            context.L(1, l);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .add(l)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, float f) {
        int arity = getArity();
        if (arity == 1) {
            context.F(1, f);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .add(f)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, double d) {
        int arity = getArity();
        if (arity == 1) {
            context.D(1, d);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .add(d)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, Object o) {
        int arity = getArity();
        if (arity == 1) {
            context.O(1, o);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .add(o)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p) {
        int arity = getArity();
        if (arity == 1) {
            context.R(2, p);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .addC(p)
                .build();
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v) {
        int arity = getArity();
        if (arity == 1) {
            context.pushFrame(new ApV());
            context.R(2, p);
            enter(context);

        } else if (arity == 2) {
            context.R(2, p);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .addC(p)
                .build();
            StgPAP pap = new StgPAP(arity - 2, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v, Object o) {
        int arity = getArity();
        if (arity == 1) {
            // TODO: Add a ApVO frame?
            context.pushFrame(new ApO(o));
            context.pushFrame(new ApV());
            context.R(2, p);
            enter(context);
        } else if (arity == 2) {
            context.pushFrame(new ApO(o));
            context.R(2, p);
            enter(context);
        } else if (arity == 3) {
            context.R(2, p);
            context.O(1, o);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .addC(p)
                .add(o)
                .build();
            StgPAP pap = new StgPAP(arity - 3, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2) {
        int arity = getArity();
        if (arity == 1) {
            context.pushFrame(new ApP(p2));
            context.R(2, p1);
            enter(context);
        } else if (arity == 2) {
            context.R(2, p1);
            context.R(3, p2);
            enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder
                .from(null)
                .addC(p1)
                .addC(p2)
                .build();
            StgPAP pap = new StgPAP(arity - 2, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPV(p2));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApV());
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .build();
                StgPAP pap = new StgPAP(arity - 3, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v, Object o) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPV(p2));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApV());
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApO(o));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.O(1, o);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .add(o)
                    .build();
                StgPAP pap = new StgPAP(arity - 4, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPP(p2, p3));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApP(p3));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .build();
                StgPAP pap = new StgPAP(arity - 3, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , Void v) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPV(p2, p3));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApPV(p3));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApV());
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .build();
                StgPAP pap = new StgPAP(arity - 4, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , Void v, Object o) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPPV(p2, p3));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPV(p3));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApV());
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 4:
                context.pushFrame(new ApO(o));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 5:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.O(1, o);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .add(o)
                    .build();
                StgPAP pap = new StgPAP(arity - 5, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPP(p2, p3, p4));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApPP(p3, p4));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApP(p4));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 4:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .addC(p4)
                    .build();
                StgPAP pap = new StgPAP(arity - 4, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4, StgClosure p5) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPPP(p2, p3, p4, p5));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApPPP(p3, p4, p5));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApPP(p4, p5));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 4:
                context.pushFrame(new ApP(p5));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                enter(context);
                break;
            case 5:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .addC(p4)
                    .addC(p5)
                    .build();
                StgPAP pap = new StgPAP(arity - 5, this, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4, StgClosure p5, StgClosure p6) {
        int arity = getArity();
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPPPP(p2, p3, p4, p5, p6));
                context.R(2, p1);
                enter(context);
                break;
            case 2:
                context.pushFrame(new ApPPPP(p3, p4, p5, p6));
                context.R(2, p1);
                context.R(3, p2);
                enter(context);
                break;
            case 3:
                context.pushFrame(new ApPPP(p4, p5, p6));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                enter(context);
                break;
            case 4:
                context.pushFrame(new ApPP(p5, p6));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                enter(context);
                break;
            case 5:
                context.pushFrame(new ApP(p6));
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                enter(context);
                break;
            case 6:
                context.R(2, p1);
                context.R(3, p2);
                context.R(4, p3);
                context.R(5, p4);
                context.R(6, p5);
                context.R(7, p6);
                enter(context);
                break;
            default:
                AbstractArgumentStack stack =
                    AbstractArgumentStack.Builder
                    .from(null)
                    .addC(p1)
                    .addC(p2)
                    .addC(p3)
                    .addC(p4)
                    .addC(p5)
                    .addC(p6)
                    .build();
                StgPAP pap = new StgPAP(arity - 6, this, stack);
                context.R(1, pap);
                break;
        }
    }
}
