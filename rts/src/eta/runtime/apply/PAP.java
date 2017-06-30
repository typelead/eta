package eta.runtime.apply;

import eta.runtime.stg.Value;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.AbstractArgumentStack;
import static eta.runtime.RuntimeLogging.barf;

public class PAP extends Value {
    public Closure fun;
    public int arity;
    public AbstractArgumentStack argStack;

    public PAP(int arity, Closure fun) {
        this(arity, fun, null);
    }

    public PAP(int arity, Closure fun, AbstractArgumentStack argStack) {
        super();
        this.fun = fun;
        this.arity = arity;
        this.argStack = argStack;
    }

    public void setStack(AbstractArgumentStack argStack) {
        this.argStack = argStack;
    }

    @Override
    public Closure enter(StgContext context) {
        barf("PAP object entered!");
        return null;
    }

    public Closure apply(StgContext context, AbstractArgumentStack stack) {
        context.merge(stack);
        return fun.enter(context);
    }

    @Override
    public Closure applyV(StgContext context) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(n)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(l)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(f)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(d)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(o)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyP(StgContext context, Closure p) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p)
            .build();
        if (arity == 1) {
            return apply(context, stack);
        } else {
            return new PAP(arity - 1, fun, stack);
        }
    }

    @Override
    public Closure applyPV(StgContext context, Closure p) {
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
                return new PAP(arity - 2, fun, stack);
        }
    }

    @Override
    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
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
                return new PAP(arity - 2, fun, builder.addC(p2).build());
        }
    }

    @Override
    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
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
                return new PAP(arity - 3, fun, builder.addC(p2).build());
        }
    }

    @Override
    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
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
                return new PAP(arity - 3, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
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
                return new PAP(arity - 4, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
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
                return new PAP(arity - 4, fun, builder.addC(p2).addC(p3).addC(p4).build());
        }
    }

    @Override
    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
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
                return new PAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
        }
    }

    @Override
    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
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
                return new PAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
        }
    }
}
