package ghcvm.runtime.message;

import ghcvm.runtime.closure.*;
import ghcvm.runtime.types.*;

public class MessageThrowTo extends Message {
    public StgTSO source;
    public StgTSO target;
    public StgClosure exception;

    public MessageThrowTo(StgTSO source, StgTSO target, StgClosure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    public void done() {
        //overwriting_closure
        //unlockClosure
    }
}
