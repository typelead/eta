package eta.runtime.exception;

public class EtaAsyncException extends StgException {

    public Closure    exception;
    public boolean    stopAtAtomically;
    public UpdateInfo stopHere;

    public EtaException(Closure exception, boolean stopAtAtomically, UpdateInfo stopHere) {
        this.exception        = exception;
        this.stopAtAtomically = stopAtAtomically;
        this.stopHere         = stopHere;
    }

}
