package ghcvm.runtime.stg;

public class ForceIO extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        context.R1.enter(context);
    }
}
