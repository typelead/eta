package eta.runtime.apply;

import eta.runtime.stg.StgClosure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.AbstractArgumentStack;
import static eta.runtime.RtsMessages.barf;

public class StgPAP extends StgClosure {
    public StgClosure fun;
    public int arity;
    public AbstractArgumentStack argStack;

    public StgPAP(int arity, StgClosure fun) {
        this(arity, fun, null);
    }

    public StgPAP(int arity, StgClosure fun, AbstractArgumentStack argStack) {
        super();
        this.fun = fun;
        this.arity = arity;
        this.argStack = argStack;
    }

    public void setStack(AbstractArgumentStack argStack) {
        this.argStack = argStack;
    }

    @Override
    public StgClosure getEvaluated() { return this; }

    @Override
    public StgClosure enter(StgContext context) {
        barf("PAP object entered!");
    }

    public StgClosure apply(StgContext context, AbstractArgumentStack stack) {
        context.merge(stack);
        return fun.enter(context);
    }

    @Override
    public StgClosure applyV(StgContext context) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyN(StgContext context, int n) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(n)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyL(StgContext context, long l) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(l)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyF(StgContext context, float f) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(f)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyD(StgContext context, double d) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(d)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyO(StgContext context, Object o) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(o)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyP(StgContext context, StgClosure p) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new StgPAP(arity - 1, fun, stack);
        }
    }

    @Override
    public StgClosure applyPV(StgContext context, StgClosure p) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p)
            .build() ;
        switch (arity) {
            case 1:
                return apply(context, stack).applyV(context);
            case 2:
                return apply(context, stack);
            default:
                return new StgPAP(arity - 2, fun, stack);
        }
    }

    @Override
    public StgClosure applyPP(StgContext context, StgClosure p1, StgClosure p2) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyP(context, p2);
            case 2:
                return apply(context, builder.addC(p2).build());
            default:
                return new StgPAP(arity - 2, fun, builder.addC(p2).build());
                break;
        }
    }

    @Override
    public StgClosure applyPPV(StgContext context, StgClosure p1, StgClosure p2) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPV(context, p2);
            case 2:
                return apply(context, builder.addC(p2).build()).applyV(context);
            case 3:
                return apply(context, builder.addC(p2).build());
            default:
                return new StgPAP(arity - 3, fun, builder.addC(p2).build());
        }
    }

    @Override
    public StgClosure applyPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPP(context, p2, p3);
            case 2:
                return apply(context, builder.addC(p2).build()).applyP(context, p3);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build());
            default:
                return new StgPAP(arity - 3, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public StgClosure applyPPPV(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPPV(context, p2, p3);
            case 2:
                return apply(context, builder.addC(p2).build()).applyPV(context, p3);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).applyV(context);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).build());
            default:
                return new StgPAP(arity - 4, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public StgClosure applyPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPPP(context, p2, p3, p4);
            case 2:
                return apply(context, builder.addC(p2).build()).applyPP(context, p3, p4);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).applyP(context, p4);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build());
            default:
                return new StgPAP(arity - 4, fun, builder.addC(p2).addC(p3).addC(p4).build());
        }
    }

    @Override
    public StgClosure applyPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPPPP(context, p2, p3, p4, p5);
            case 2:
                return apply(context, builder.addC(p2).build()).applyPPP(context, p3, p4, p5);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).applyPP(context, p4, p5);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build()).applyP(context, p5);
            case 5:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
            default:
                return new StgPAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
        }
    }

    @Override
    public StgClosure applyPPPPPP(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3, StgClosure p4, StgClosure p5, StgClosure p6) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).applyPPPPP(context, p2, p3, p4, p5, p6);
            case 2:
                return apply(context, builder.addC(p2).build()).applyPPPP(context, p3, p4, p5, p6);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).applyPPP(context, p4, p5, p6);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build()).applyPP(context, p5, p6);
            case 5:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).build()).applyP(context, p6);
            case 6:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
            default:
                return new StgPAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
        }
    }
}
