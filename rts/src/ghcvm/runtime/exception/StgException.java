package ghcvm.runtime.exception;

import ghcvm.runtime.closure.StgClosure;

public class StgException extends RuntimeException {
    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    public static StgClosure raise = null; /* TODO: Implement */

    @Override
    public Throwable fillInStackTrace() {
        return null;
    }
}
