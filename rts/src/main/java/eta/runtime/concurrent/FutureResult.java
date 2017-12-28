package eta.runtime.concurrent;

public class FutureResult {
    public final Object    result;
    public final Exception exception;

    public FutureResult(Object result, Exception exception) {
        this.result    = result;
        this.exception = exception;
    }
}
