package ghcvm.runtime.exception;

public class StgException extends RuntimeException {
    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    @Override
    public Throwable fillInStackTrace() {
        return null;
    }
}
