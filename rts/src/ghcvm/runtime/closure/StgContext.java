package ghcvm.runtime.closure;

import java.util.Iterator;
import ghcvm.runtime.types.*;
import static ghcvm.runtime.types.StgTSO.*;

public class StgContext {
    public StgClosure R2;
    public int I1;
    public float F1;
    public double D1;
    public char C1;
    public byte B1;
    public short S1;
    public String Str1;
    public StgTSO currentTSO;
    public Capability myCapability;
    public ReturnCode ret;
    public Iterator<StackFrame> it;
}
