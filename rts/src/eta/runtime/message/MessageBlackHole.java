package eta.runtime.message;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.Thunk;

public final class MessageBlackHole extends Message {

    public final TSO tso;
    public final Thunk bh;

    public MessageBlackHole(final TSO tso, final Thunk bh) {
        this.tso = tso;
        this.bh = bh;
    }

    @Override
    public void execute(Capability cap) {
        boolean blocked = cap.messageBlackHole(bh);
        if (!blocked) {
            cap.tryWakeupThread(tso);
        }
    }
}
