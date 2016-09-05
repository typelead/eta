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
    public void apply(StgContext context, StgClosure p) {
        int arity = getArity();
        if (arity == 1) {
            context.R(2, p);
            enter(context);
        } else {
            AbstractArgumentStack stack = new SimpleArgumentStack();
            stack.R(1, p);
            StgPAP pap = new StgPAP(arity - 1, this, stack);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p1, StgClosure p2) {
        int arity = getArity();
        if (arity == 2) {
            context.R(2, p1);
            context.R(3, p2);
            enter(context);
        } else {
            AbstractArgumentStack stack = new SimpleArgumentStack();
            stack.R(1, p1);
            stack.R(2, p2);
            StgPAP pap = new StgPAP(arity - 2, this, stack);
            context.R(1, pap);
        }
    }
}
