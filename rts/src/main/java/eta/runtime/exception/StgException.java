package eta.runtime.exception;

public class StgException extends RuntimeException {

    /* This makes throwing exceptions fast. */
    @Override
    public Throwable fillInStackTrace() {
        return null;
    }
}
