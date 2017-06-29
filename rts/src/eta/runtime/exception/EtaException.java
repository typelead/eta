package eta.runtime.exception;

import eta.runtime.stg.Closure;

public class EtaException extends StgException {

    public Closure exception;

    public EtaException(Closure exception) {
        this.exception = exception;
    }

}
