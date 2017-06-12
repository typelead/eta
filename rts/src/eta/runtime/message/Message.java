package eta.runtime.message;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import static eta.runtime.RuntimeLogging.barf;

public abstract class Message extends Value {
    protected volatile boolean valid = true;
    public boolean isValid() { return valid; }
    public boolean isLocked() { return false; }
    public void invalidate() { valid = false; }
    public abstract void execute(Capability cap);
}
