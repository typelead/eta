package eta.runtime.thunk;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.Queue;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.TSO;
import eta.runtime.thunk.Thunk;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.RuntimeLogging.barf;

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
