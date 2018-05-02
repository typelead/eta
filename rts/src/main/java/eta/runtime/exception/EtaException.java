package eta.runtime.exception;

import java.util.Arrays;

import eta.runtime.Runtime;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;

public class EtaException extends StgException {

    public Closure exception;

    protected EtaException(Closure exception) {
        this.exception = exception;
    }

    @Override
    public String getLocalizedMessage() {
        return Closures.showException(exception);
    }

    public static EtaException create(StgContext context, Closure exception) {
        final EtaException e = new EtaException(exception);
        final StackTraceElement[] original = Thread.currentThread().getStackTrace();
        if (Runtime.debugExceptions()) {
            e.setStackTrace(original);
        } else {
            e.setStackTrace(Arrays.copyOfRange(original, 3, original.length));
        }
        context.setCauseAndException(e, exception);
        return e;
    }
}
