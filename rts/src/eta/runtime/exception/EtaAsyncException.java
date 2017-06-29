package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.thunk.UpdateInfo;

public class EtaAsyncException extends StgException {

    public Closure    exception;
    public boolean    stopAtAtomically;
    public UpdateInfo stopHere;

    public EtaAsyncException(Closure exception, boolean stopAtAtomically, UpdateInfo stopHere) {
        this.exception        = exception;
        this.stopAtAtomically = stopAtAtomically;
        this.stopHere         = stopHere;
    }

}
