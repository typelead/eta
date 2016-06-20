package ghcvm.runtime.message;

import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.stg.StgClosure;
import static ghcvm.runtime.RtsMessages.barf;

public abstract class Message extends StgClosure {
    protected volatile boolean valid = true;
    public boolean isValid() { return valid; }
    public boolean isLocked() { return false; }
    public void invalidate() { valid = false; }
    public void execute(Capability cap) {
        barf("executeMessage: %p", this);
    }
}
