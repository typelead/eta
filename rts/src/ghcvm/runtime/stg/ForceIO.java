package ghcvm.runtime.stg;

public class ForceIO extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgClosure ret = context.R(1);
        ret.enter(context);
    }
}
