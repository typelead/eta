package ghcvm.runtime.message;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.closure.StgClosure;

public class MessageBlackHole extends Message {

    public StgTSO tso;
    public StgClosure bh;

    public MessageBlackHole(StgTSO tso, StgClosure bh) {
        this.tso = tso;
        this.bh = bh;
    }
}
