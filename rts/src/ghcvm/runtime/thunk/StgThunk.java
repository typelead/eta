package ghcvm.runtime.thunk;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.apply.Void;
import ghcvm.runtime.apply.ApV;
import static ghcvm.runtime.RtsMessages.barf;

public class StgThunk extends StgClosure {
    public volatile StgClosure indirectee;

    public StgThunk() {
        this(null);
    }

    public StgThunk(StgClosure indirectee) {
        super();
        this.indirectee = indirectee;
    }

    @Override
    public final StgClosure getEvaluated() {
        if (indirectee == null) return null;
        else return indirectee.getEvaluated();
    }

    public void thunkEnter(StgContext context) {
        barf("thunkEnter not implemented");
    }

    public final void updateWithIndirection(StgClosure ret) {
        indirectee = ret;
    }

    @Override
    public final void apply(StgContext context, Void v) {
        if (indirectee == null) {
            StgTSO tso = context.currentTSO;
            tso.sp.add(new ApV());
            enter(context);
        } else {
            indirectee.apply(context, v);
        }
    }
}
