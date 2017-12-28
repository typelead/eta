package eta.runtime.exception;

public class RetryException extends StgException {
    public static RetryException INSTANCE = new RetryException();
    public RetryException() {}
}
