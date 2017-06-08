package eta.runtime.message;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import static eta.runtime.RtsMessages.barf;

public abstract class Message extends StgValue {
    protected volatile boolean valid = true;
    public boolean isValid() { return valid; }
    public boolean isLocked() { return false; }
    public void invalidate() { valid = false; }
    public void execute(Capability cap) {
        barf("executeMessage: %p", this);
    }
}
