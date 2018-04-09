package eta.runtime.exception;

import eta.Closure;
import eta.runtime.stg.Closures;
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

    @Override
    public String getLocalizedMessage() {
        return Closures.showException(exception);
    }
}
