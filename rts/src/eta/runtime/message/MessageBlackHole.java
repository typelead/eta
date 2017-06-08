package eta.runtime.message;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.Thunk;

public final class MessageBlackHole extends Message {

    public final TSO tso;
    public final Thunk bh;
    public final WeakReference<Thread> thread;

    public MessageBlackHole(final TSO tso, final Thunk bh, final Thread t) {
        this.tso = tso;
        this.bh = bh;
        this.thread = new WeakReference<Thead>(t);
    }
}
