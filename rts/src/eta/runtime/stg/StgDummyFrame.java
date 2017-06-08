package eta.runtime.stg;

public class StgDummyFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {}

    @Override
    public Closure getClosure() { return new StgDummyRet(); }
}
