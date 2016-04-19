package ghcvm.runtime.closure;

import ghcvm.runtime.types.*;
import static ghcvm.runtime.types.StgTSO.*;

public class StgContext {
    public StgClosure R1;
    public int I1;
    public float F1;
    public double D1;
    public char C1;
    public byte B1;
    public StgTSO currentTSO;
    public Capability myCapability;
    public ReturnCode ret;
}
