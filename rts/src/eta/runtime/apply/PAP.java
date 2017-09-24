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
    public Closure apply1(StgContext context, Closure p) {
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
    public Closure apply1V(StgContext context, Closure p) {
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
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply1(context, p2);
            case 2:
                return apply(context, builder.addC(p2).build());
            default:
                return new PAP(arity - 2, fun, builder.addC(p2).build());
        }
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply1V(context, p2);
            case 2:
                return apply(context, builder.addC(p2).build()).applyV(context);
            case 3:
                return apply(context, builder.addC(p2).build());
            default:
                return new PAP(arity - 3, fun, builder.addC(p2).build());
        }
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply2(context, p2, p3);
            case 2:
                return apply(context, builder.addC(p2).build()).apply1(context, p3);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build());
            default:
                return new PAP(arity - 3, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply2V(context, p2, p3);
            case 2:
                return apply(context, builder.addC(p2).build()).apply1V(context, p3);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).applyV(context);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).build());
            default:
                return new PAP(arity - 4, fun, builder.addC(p2).addC(p3).build());
        }
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply3(context, p2, p3, p4);
            case 2:
                return apply(context, builder.addC(p2).build()).apply2(context, p3, p4);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).apply1(context, p4);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build());
            default:
                return new PAP(arity - 4, fun, builder.addC(p2).addC(p3).addC(p4).build());
        }
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply4(context, p2, p3, p4, p5);
            case 2:
                return apply(context, builder.addC(p2).build()).apply3(context, p3, p4, p5);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).apply2(context, p4, p5);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build()).apply1(context, p5);
            case 5:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
            default:
                return new PAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
        }
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                return apply(context, builder.build()).apply5(context, p2, p3, p4, p5, p6);
            case 2:
                return apply(context, builder.addC(p2).build()).apply4(context, p3, p4, p5, p6);
            case 3:
                return apply(context, builder.addC(p2).addC(p3).build()).apply3(context, p4, p5, p6);
            case 4:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).build()).apply2(context, p5, p6);
            case 5:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).build()).apply1(context, p6);
            case 6:
                return apply(context, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
            default:
                return new PAP(arity - 5, fun, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
        }
    }
}
