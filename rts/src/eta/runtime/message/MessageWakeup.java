package eta.runtime.message;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;

public final class MessageWakeup extends Message {

    public final StgTSO tso;

    public MessageWakeup(final StgTSO tso) {
        this.tso = tso;
    }

    @Override
    public final void execute(Capability cap) {
        cap.tryWakeupThread(tso);
    }
}
