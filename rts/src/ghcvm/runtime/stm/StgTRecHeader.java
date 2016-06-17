package ghcvm.runtime.stm;

import java.util.Queue;
import java.util.ArrayDeque;

public class StgTRecHeader {
    public StgTRecChunk currentChunk;
    public Queue<StgInvariantCheck> invariantsToCheck = new ArrayDeque<StgInvariantCheck>();
    public TRecState state;
}
