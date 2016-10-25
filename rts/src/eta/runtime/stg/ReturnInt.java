package eta.runtime.stg;

public class ReturnInt extends StackFrame {
    public final int n;

    public ReturnInt(int n) {
        this.n = n;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.I(1, n);
    }
}
