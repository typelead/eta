package eta.runtime.thunk;

import eta.runtime.stg.StgContext;

public abstract class StgInd extends StgThunk {

    @Override
    public void enter(StgContext context) {
        super.enter(context);
        if (indirectee == null) {
            context.pushFrame(new StgUpdateFrame(this));
            thunkEnter(context);
        } else {
            Thunk.blackHole(context, this, indirectee);
        }
    }
}
