package ghcvm.runtime.closure;

import java.util.ListIterator;

import ghcvm.runtime.stg.*;
import ghcvm.runtime.stg.*;

public class StgContext {
    public StgClosure R1;
    public StgClosure R2;
    public StgClosure R3;
    public StgClosure R4;
    public StgClosure R5;
    public StgClosure R6;
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
    public int papExpectedArity;

    public enum ReturnCode {
        HeapOverflow,
        StackOverflow,
        ThreadYielding,
        ThreadBlocked,
        ThreadFinished
    }
}
