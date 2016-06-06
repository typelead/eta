package ghcvm.runtime.message;

import ghcvm.runtime.types.StgTSO;

public class MessageWakeup extends Message {

    public StgTSO tso;

    public MessageWakeup(StgTSO tso) {
        this.tso = tso;
    }
}
