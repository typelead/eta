package eta.runtime.message;

import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.StgThunk;

public final class MessageBlackHole extends Message {

    public final StgTSO tso;
    public final StgThunk bh;
    public final WeakReference<Thread> thread;

    public MessageBlackHole(final StgTSO tso, final StgThunk bh, final Thread t) {
        this.tso = tso;
        this.bh = bh;
        this.thread = new WeakReference<Thead>(t);
    }
}
