package ghcvm.runtime.stg;

public class StgDummyFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {}

    @Override
    public StgClosure getClosure() { return new StgDummyRet(); }
}
