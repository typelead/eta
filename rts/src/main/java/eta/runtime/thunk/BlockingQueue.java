package eta.runtime.thunk;


import eta.runtime.stg.TSO;

public class BlockingQueue extends BlackHole {
    public final TSO   owner;
    public final Thunk bh;
    public TSO queued;

    public BlockingQueue(final TSO owner, final Thunk bh, final TSO queued) {
        this.owner    = owner;
        this.bh       = bh;
        this.queued   = queued;
        queued.link   = null;
    }

    public final void queue(TSO tso) {
        tso.link = queued;
        queued = tso;
    }

}
