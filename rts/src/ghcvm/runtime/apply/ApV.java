package ghcvm.runtime.apply;

#include "Rts.h"

import ghcvm.runtime.*;
import ghcvm.runtime.closure.*;
import static ghcvm.runtime.RtsMessages.*;

public class ApV extends StackFrame {

    @Override
    public void enter(StgContext context) {
        IF_DEBUG(DEBUG_APPLY, debugBelch("stg_ap_v_ret... "); printClosure(context.R1));
        // Some more code here
    }
}
