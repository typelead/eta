package ghcvm.runtime.thunk;

public class Thunk {
    public static StgClosure block_blackhole = new StgClosure() {
            @Override
            public void enter(StgContext context) {
                StgTSO tso = context.currentTSO;
                StgClosure closure = context.R1;
                ListIterator<StackFrame> sp = tso.sp;
                sp.add(new StgEnter(closure));
                tso.whatNext = ThreadRunGHC;
                context.ret = ThreadBlocked;
                Stg.returnToSched.enter(context);
            }
        };
}
