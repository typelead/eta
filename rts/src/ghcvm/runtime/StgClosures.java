package ghcvm.runtime;

import ghcvm.runtime.types.*;
import static ghcvm.runtime.types.StgClosure.ClosureType.*;

public class StgClosures {
    public static StgClosure stg_ap_v = new StackFrame() {
            ClosureType closureType = RetSmall;
            public void enter() {}
        };
    public static StgClosure stg_enter = new StackFrame() {
            ClosureType closureType = RetSmall;
            StgClosure closure;
            public void enter() {
                closure.enter();
            }
        };
}
