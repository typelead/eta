package eta.runtime.message;

import eta.runtime.stg.Capability;

public abstract class Message {
    protected volatile boolean valid = true;
    public boolean isValid() { return valid; }
    public boolean isLocked() { return false; }
    public void invalidate() { valid = false; }
    public abstract void execute(Capability cap);
}
