package ghcvm.runtime.message;

import ghcvm.runtime.types.Capability;
import ghcvm.runtime.closure.StgClosure;
import static ghcvm.runtime.RtsMessages.barf;

public abstract class Message extends StgClosure {
    protected boolean valid = true;
    public boolean isValid() { return valid; }
    public void invalidate() { valid = false; }
    public void execute(Capability cap) {
        barf("executeMessage: %p", this);
    }
}
