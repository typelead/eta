package ghcvm.runtime;

import ghcvm.runtime.Capability;
import ghcvm.runtime.SchedulerStatus;

public class Rts {
    public static Capability lock() {return null;}
    public static void unlock(Capability cap) {}
    public static void evalLazyIO(Ptr<Capability> cap, CLOSURE_PTR p, REF_CLOSURE_PTR ret) {}
    public static SchedulerStatus getSchedStatus(Capability cap) {return null;}
}
