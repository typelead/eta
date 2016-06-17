package ghcvm.runtime.stm;

import java.util.concurrent.atomic.AtomicLong;

import ghcvm.runtime.closure.StgClosure;

public class StgAtomicInvariant {
    public StgClosure code;
    public StgTRecHeader lastExecution;
    public AtomicLong lock;
}
