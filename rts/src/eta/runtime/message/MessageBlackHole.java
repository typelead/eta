package eta.runtime.message;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.StgThunk;

public final class MessageBlackHole extends Message {

    public final StgTSO tso;
    public final StgThunk bh;

    public MessageBlackHole(final StgTSO tso, final StgThunk bh) {
        this.tso = tso;
        this.bh = bh;
    }

    @Override
    public final void execute(Capability cap) {
        boolean blocked = cap.messageBlackHole(this);
        if (!blocked) {
            cap.tryWakeupThread(tso);
        }
    }
}
