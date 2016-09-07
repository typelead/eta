package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.AbstractArgumentStack;
import ghcvm.runtime.stg.SimpleArgumentStack;
import static ghcvm.runtime.RtsMessages.barf;

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
    public void enter(StgContext context) {
        barf("PAP object entered!");
    }

    @Override
    public void apply(StgContext context, Void v) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, int n) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(n)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, long l) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(l)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, float f) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(f)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, double d) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(d)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, Object o) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .add(o)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p)
            .build();
        if (arity == 1) {
            context.R(1, this);
            context.O(1, stack);
            Apply.PAP_apply.enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p)
            .build() ;
        switch (arity) {
            case 1:
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, stack);
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.R(1, this);
                context.O(1, stack);
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 2, fun, stack);
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p, Void v, Object o) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p);
        switch (arity) {
            case 1:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApO(o));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.R(1, this);
                context.O(1, builder.add(o).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 3, fun, builder.add(o).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApP(p2));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 2, fun, builder.addC(p2).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPV(p2));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 3, fun, builder.addC(p2).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, Void v, Object o) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPV(p2));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApO(o));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.R(1, this);
                context.O(1, builder.addC(p2).add(o).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 4, fun, builder.addC(p2).add(o).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPP(p2, p3));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApP(p3));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 3, fun, builder.addC(p2).addC(p3).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , Void v) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPV(p2, p3));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApPV(p3));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 4, fun, builder.addC(p2).addC(p3).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , Void v, Object o) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPPV(p2, p3));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApPV(p3));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApO(o));
                context.pushFrame(new ApV());
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.pushFrame(new ApO(o));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 5:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).add(o).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 5, fun, builder.addC(p2).addC(p3)
                                                               .add(o).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPP(p2, p3, p4));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApPP(p3, p4));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApP(p4));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 4, fun, builder.addC(p2).addC(p3)
                                                               .addC(p4).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4, StgClosure p5) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPPP(p2, p3, p4, p5));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApPPP(p3, p4, p5));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApPP(p4, p5));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.pushFrame(new ApP(p5));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).build());
                Apply.PAP_apply.enter(context);
                break;
            case 5:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 5, fun, builder.addC(p2).addC(p3)
                                                               .addC(p4).addC(p5).build());
                context.R(1, pap);
                break;
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2, StgClosure p3
                      , StgClosure p4, StgClosure p5, StgClosure p6) {
        AbstractArgumentStack.Builder builder =
            AbstractArgumentStack.Builder
            .from(argStack)
            .addC(p1);
        switch (arity) {
            case 1:
                context.pushFrame(new ApPPPPP(p2, p3, p4, p5, p6));
                context.R(1, this);
                context.O(1, builder.build());
                Apply.PAP_apply.enter(context);
                break;
            case 2:
                context.pushFrame(new ApPPPP(p3, p4, p5, p6));
                context.R(1, this);
                context.O(1, builder.addC(p2).build());
                Apply.PAP_apply.enter(context);
                break;
            case 3:
                context.pushFrame(new ApPPP(p4, p5, p6));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).build());
                Apply.PAP_apply.enter(context);
                break;
            case 4:
                context.pushFrame(new ApPP(p5, p6));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).build());
                Apply.PAP_apply.enter(context);
                break;
            case 5:
                context.pushFrame(new ApP(p6));
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).addC(p5).build());
                Apply.PAP_apply.enter(context);
                break;
            case 6:
                context.R(1, this);
                context.O(1, builder.addC(p2).addC(p3).addC(p4).addC(p5).addC(p6).build());
                Apply.PAP_apply.enter(context);
                break;
            default:
                StgPAP pap = new StgPAP(arity - 5, fun, builder.addC(p2).addC(p3)
                                                               .addC(p4).addC(p5)
                                                               .addC(p6).build());
                context.R(1, pap);
                break;
        }
    }

}
