package ghcvm.runtime.prim;

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
        context.R1 = weak;
    }
}
