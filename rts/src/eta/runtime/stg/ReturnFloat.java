package eta.runtime.stg;

public class ReturnFloat extends StackFrame {
    public final float f;

    public ReturnFloat(float f) {
        this.f = f;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.F(1, f);
    }
}
