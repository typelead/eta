package ghcvm.runtime.apply;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.AbstractArgumentStack;
import ghcvm.runtime.stg.SimpleArgumentStack;

public class StgFun extends StgClosure {
    public int arity;

    public StgFun(int arity) {
        this.arity = arity;
    }

    @Override
    public void enter(StgContext context) {
        context.R(1, this);
    }

    @Override
    public void apply(StgContext context, Void v) {
        if (arity == 1) {
            enter(context);
        } else {
            StgPAP pap = new StgPAP(arity - 1, this);
            context.R(1, pap);
        }
    }

    @Override
    public void apply(StgContext context, StgClosure p) {
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
}
