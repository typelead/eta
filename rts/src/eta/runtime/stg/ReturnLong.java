package eta.runtime.stg;

public class ReturnLong extends StackFrame {
    public final long l;

    public ReturnLong(long l) {
        this.l = l;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.L(1, l);
    }
}
