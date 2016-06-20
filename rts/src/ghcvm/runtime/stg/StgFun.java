package ghcvm.runtime.stg;

public class StgFun extends StgClosure {
    public byte arity;
    public StgClosure fun;

    public StgFun(StgClosure fun, byte arity) {
        this.fun = fun;
        this.arity = arity;
    }

    @Override
    public void enter(StgContext context) {
        context.R1 = this;
    }

    @Override
    public void preEnter(StgContext context) {
        if (arity == 1) {
            context.R1.enter(context);
        } else {
            int expectedArity = context.papExpectedArity;
            StgPAP pap = new StgPAP(context.R1, (byte) (arity - expectedArity), (byte) 0 /* argument signature here */);
            // Code to copy expected arguments over to pap
            context.R1 = pap;
        }
    }
}
