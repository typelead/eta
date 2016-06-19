package ghcvm.runtime.message;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;

public final class MessageWakeup extends Message {

    public final StgTSO tso;

    public MessageWakeup(final StgTSO tso) {
        this.tso = tso;
    }

    @Override
    public final void execute(Capability cap) {
        //write barrier
        cap.tryWakeupThread(tso);
    }
}
