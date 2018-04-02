package eta.exception;

import eta.Closure;
import eta.runtime.exception.StgException;
import eta.runtime.stg.Closures;

public class EtaException extends StgException {

    public Closure exception;

    public EtaException(Closure exception) {
        this.exception = exception;
    }

    @Override
    public String getLocalizedMessage() {
        return Closures.showException(exception);
    }
}
