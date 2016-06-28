package ghcvm.runtime.stg;

public class ReturnObject extends StackFrame {
    public final Object object;

    public ReturnObject(final Object object) {
        this.object = object;
    }

    @Override
    public void stackEnter(StgContext context) {
        context.O(1, object);
    }
}
