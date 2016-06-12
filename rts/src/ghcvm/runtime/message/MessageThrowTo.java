package ghcvm.runtime.message;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.types.StgTSO;

public class MessageThrowTo extends Message {
    public final StgTSO source;
    public final StgTSO target;
    public final StgClosure exception;

    public MessageThrowTo(final StgTSO source, final StgTSO target, final StgClosure exception) {
        this.source = source;
        this.target = target;
        this.exception = exception;
    }

    public void done() {
        //overwriting_closure
        //unlockClosure
    }
}
