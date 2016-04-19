package ghcvm.runtime.exception;

public class StgException extends RuntimeException {
    @Override
    public Throwable fillInStackTrace() {
        return null;
    }
}
