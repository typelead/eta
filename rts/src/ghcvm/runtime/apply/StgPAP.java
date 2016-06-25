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
        if (arity == 1) {
            context.R(1, this);
            Apply.PAP_apply.enter(context);
        } else {
            AbstractArgumentStack stack =
                AbstractArgumentStack.Builder.from(argStack)
                .build();
            StgPAP pap = new StgPAP(arity - 1, fun, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p) {
        if (arity == 1) {
            enter(context);
        } else {
            AbstractArgumentStack stack = new SimpleArgumentStack();
            stack.R(1, p);
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }
}
