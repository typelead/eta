package eta.runtime.stg;

public class ForceIO extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        Closure ret = context.R(1);
        ret.enter(context);
    }
}
