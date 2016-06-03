package ghcvm.runtime;

import ghcvm.runtime.prim.*;
import ghcvm.runtime.apply.*;
import ghcvm.runtime.closure.*;

public class Stg {
    public static StackFrame ap_v = new ApV();

    public static StgClosure mkWeakzh = new MkWeak();
}
