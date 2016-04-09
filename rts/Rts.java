package ghcvm.runtime;

import ghcvm.runtime.Capability;

public class Rts {
    public static Capability lock() {return null;}
    // TODO: types are inaccurate
    public static void evalLazyIO(Capability cap, CLOSURE_PTR p, CLOSURE_PTR ret) {}
}
