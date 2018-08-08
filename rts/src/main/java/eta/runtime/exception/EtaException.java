package eta.runtime.exception;

import java.util.Map;
import java.util.Arrays;
import java.util.Collections;
import java.util.WeakHashMap;

import eta.runtime.Runtime;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import static eta.runtime.RuntimeLogging.*;

public class EtaException extends StgException {

    public Closure exception;

    protected EtaException(Closure exception) {
        this.exception = exception;
    }

    @Override
    public String getLocalizedMessage() {
        return Closures.showException(exception);
    }

    private static Map<Closure, java.lang.Exception> jexceptionMapping =
        Collections.synchronizedMap(new WeakHashMap<Closure, java.lang.Exception>());

    public static EtaException create(StgContext context, Closure exception) {
        java.lang.Exception oldCause = context.getCause();
        EtaException e = null;
        if (oldCause == null) {
            /* If no previous exception was thrown in the current chain. */
            e = createInternal(exception);
            java.lang.Exception oldException = jexceptionMapping.get(exception);
            if (oldException != null) {
                e.initCause(oldException);
            }
        } else {
            Closure lastException = context.getException();
            if (lastException != exception) {
                /* If the last exception thrown in the chain was different
                   than the one being thrown now, create a cause chain. */
                e = createInternal(exception);
                e.initCause(oldCause);
            } else {
                e = (EtaException) oldCause;
            }
        }
        context.setCauseAndException(e, exception);
        return e;
    }

    private static EtaException createInternal(Closure exception) {
        final EtaException e = new EtaException(exception);
        final StackTraceElement[] original = Thread.currentThread().getStackTrace();
        if (Runtime.debugExceptions()) {
            e.setStackTrace(original);
        } else {
            e.setStackTrace(Arrays.copyOfRange(original, 4, original.length));
        }
        return e;
    }

    public static Closure convertJavaException(TSO tso, java.lang.Exception e) {
        if (Runtime.debugExceptions()) {
            debugExceptions("Converting Java Exception: " + e);
            debugExceptions(Exception.exceptionToString(e));
        }
        tso.setCauseAndException(e, null);
        Closure jexception = Closures.mkSomeException(e);
        jexceptionMapping.put(jexception, e);
        return jexception;
    }

    public static EtaException fromJavaException(TSO tso, java.lang.Exception e) {
        return EtaException.create(tso.getContext(), convertJavaException(tso, e));
    }
}
