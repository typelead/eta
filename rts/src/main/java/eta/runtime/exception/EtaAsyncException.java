package eta.runtime.exception;

import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.thunk.Thunk;

public class EtaAsyncException extends StgException {

    public Closure    exception;
    public boolean    stopAtAtomically;
    public Thunk stopHere;

    public EtaAsyncException(Closure exception, boolean stopAtAtomically, Thunk stopHere) {
        this.exception        = exception;
        this.stopAtAtomically = stopAtAtomically;
        this.stopHere         = stopHere;
    }

    @Override
    public String getLocalizedMessage() {
        return Closures.showException(exception);
    }
}
