package eta.runtime.stg;

public class ReturnDouble extends StackFrame {
    public final double d;

    public ReturnDouble(double d) {
        this.d = d;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.D(1, d);
    }
}
