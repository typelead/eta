package ghcvm.runtime.exception;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StackFrame;
import ghcvm.runtime.stg.StgContext;
import static ghcvm.runtime.stg.StgTSO.TSO_BLOCKEX;
import static ghcvm.runtime.stg.StgTSO.TSO_INTERRUPTIBLE;

public class MaskUninterruptibleFrame extends StackFrame {

    @Override
    public void stackEnter(StgContext context) {
        StgTSO tso = context.currentTSO;
        tso.addFlags(TSO_BLOCKEX);
        tso.removeFlags(TSO_INTERRUPTIBLE);
    }
}
