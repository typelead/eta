package ghcvm.runtime.exception;

import ghcvm.runtime.closure.StgClosure;

public class StgException extends RuntimeException {

    @Override
    public Throwable fillInStackTrace() {
        return null;
    }

    public static StgException stgReturnException = new StgReturnException();
    public static StgException threadYieldException = new ThreadYieldException();
    public static StgException stackReloadException = new StackReloadException();

    public static StgClosure getMaskingState = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                context.I1 = ((tso.hasFlag(TSO_BLOCKEX)? 1: 0) +
                              (tso.hasFlag(TSO_INTERRUPTIBLE)? 1: 0));
            }
        };

    public static StgClosure raise = null; /* TODO: Implement */
}
