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
        if (!cap.messageBlackHole(bh, tso, true)) {
            cap.tryWakeupThread(tso);
        }
    }

    @Override
    public String toString() {
        return "MessageBlackHole[tso=" + tso + ",thunk=" + bh + "]";
    }
}
