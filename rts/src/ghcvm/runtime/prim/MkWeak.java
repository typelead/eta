package ghcvm.runtime.prim;

#include "Rts.h"

import ghcvm.runtime.*;
import ghcvm.runtime.closure.*;

import static ghcvm.runtime.RtsMessages.*;

public class MkWeak extends StgClosure {
    @Override
    public void enter(StgContext context) {
        StgClosure key = context.R2;
        StgClosure value = context.R3;
        StgClosure finalizer = context.R4;
        StgWeak weak = new StgWeak(key, value, finalizer);
        context.myCapability.weakPtrList.add(weak);

        IF_DEBUG(DEBUG_WEAK, debugBelch("New weak pointer at %s\n", weak));
        context.R1 = weak;
        context.R2 = null;
        context.R3 = null;
        context.R4 = null;
        /* The null sets are added to ensure that the references don't cause
           memory leaks */
    }
}
