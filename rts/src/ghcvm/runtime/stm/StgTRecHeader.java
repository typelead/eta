package ghcvm.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;

import ghcvm.runtime.closure.StgClosure;

public class StgTRecHeader extends StgClosure {
    public Stack<StgTRecChunk> chunkStack;
    public Queue<StgInvariantCheck> invariantsToCheck = new ArrayDeque<StgInvariantCheck>();
    public TRecState state;

    @Override
    public boolean isTrecHeader() { return true; }
}
