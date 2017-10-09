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

public class BlockingQueue extends BlackHole implements Iterable<MessageBlackHole> {
    public TSO owner;
    public Thunk bh;
    public final Queue<MessageBlackHole> messages;

    public BlockingQueue(final TSO owner, final MessageBlackHole msg) {
        this.owner    = owner;
        this.bh       = msg.bh;
        this.messages = new ArrayDeque<MessageBlackHole>();
        messages.offer(msg);
    }

    @Override
    public final Iterator<MessageBlackHole> iterator() {
        return messages.iterator();
    }

    public final void clear() {
        owner   = null;
        bh      = null;
        messages.clear();
    }

}
