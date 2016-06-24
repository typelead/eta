package ghcvm.runtime.message;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.thunk.StgThunk;

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
