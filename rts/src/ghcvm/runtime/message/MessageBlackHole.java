package ghcvm.runtime.message;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;

public final class MessageBlackHole extends Message {

    public final StgTSO tso;
    public final StgClosure bh;

    public MessageBlackHole(final StgTSO tso, final StgClosure bh) {
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
