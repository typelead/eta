package ghcvm.runtime.stg;

import java.lang.ref.WeakReference;

import ghcvm.runtime.closure.*;
import static ghcvm.runtime.RtsMessages.*;

public class StgWeak extends StgClosure {
    // TODO: Is this the right reference type?
    public WeakReference<StgClosure> key;
    public WeakReference<StgClosure> value;
    // TODO: Should the finalizer be a weak reference as well?
    public StgClosure finalizer;
    public StgClosure[] cfinalizers;

    public StgWeak(StgClosure key, StgClosure value, StgClosure finalizer) {
        this.key = new WeakReference<StgClosure>(key);
        this.value = new WeakReference<StgClosure>(value);
        this.finalizer = finalizer;
    }

    @Override
    public void enter(StgContext context) {
        barf("WEAK object entered!");
    }
}
