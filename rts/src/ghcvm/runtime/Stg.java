package ghcvm.runtime;

import ghcvm.runtime.prim.*;
import ghcvm.runtime.apply.*;
import ghcvm.runtime.closure.*;
import ghcvm.runtime.stg.*;

public class Stg {
    public static StackFrame ap_v = new ApV();

    public static StgClosure mkWeakzh = new MkWeak();
    public static StgClosure PAP_apply = new PAPApply();
    public static StgClosure block_blackhole = new BlockBlackhole();
    public static StgClosure returnToSched = new ReturnToScheduler();
}
