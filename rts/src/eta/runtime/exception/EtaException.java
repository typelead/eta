package eta.runtime.exception;

public class EtaException extends StgException {

    public Closure exception;

    public EtaException(Closure exception) {
        this.exception = exception;
    }

}
