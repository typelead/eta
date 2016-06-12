package ghcvm.runtime;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.stg.*;
import ghcvm.runtime.apply.*;
import ghcvm.runtime.prim.*;
import ghcvm.runtime.stackframe.*;

public class Stg {
    public static StackFrame ap_v = new ApV();

    public static StgClosure mkWeakzh = new MkWeak();
    public static StgClosure PAP_apply = new PAPApply();
    public static StgClosure block_blackhole = new BlockBlackHole();
    public static StgClosure returnToSched = new ReturnToScheduler();
    public static StgClosure returnToSchedButFirst = new ReturnToSchedulerButFirst();
    public static StgClosure readMVar = new ReadMVar();
    public static StgClosure putMVar = new PutMVar();
    public static StgClosure block_readmvar = new BlockReadMVar();
    public static StgClosure block_readmvar_finally = new BlockReadMVarFinally();
    public static StgClosure block_putmvar = new BlockPutMVar();
    public static StgClosure noDuplicate = new NoDuplicate();
    public static StgClosure threadFinished = new ThreadFinished();
}
