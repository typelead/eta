package ghcvm.runtime.apply;

import cern.colt.list.AbstractDoubleList;
import cern.colt.list.DoubleArrayList;
import cern.colt.list.AbstractFloatList;
import cern.colt.list.FloatArrayList;
import cern.colt.list.AbstractIntList;
import cern.colt.list.IntArrayList;
import cern.colt.list.AbstractLongList;
import cern.colt.list.LongArrayList;
import cern.colt.list.AbstractList;
import cern.colt.list.ObjectArrayList;

import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.AbstractArgumentStack;
import ghcvm.runtime.stg.SimpleArgumentStack;
import static ghcvm.runtime.RtsMessages.barf;

public class StgPAP extends StgFun {
    public StgClosure fun;
    public AbstractArgumentStack argStack;

    public StgPAP(int arity, StgClosure fun) {
        this(arity, fun, null);
    }

    public StgPAP(int arity, StgClosure fun, AbstractArgumentStack argStack) {
        super(arity);
        this.fun = fun;
        this.argStack = argStack;
    }

    public void setStack(AbstractArgumentStack argStack) {
        this.argStack = argStack;
    }

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
