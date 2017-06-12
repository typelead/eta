package eta.runtime.stg;

import java.util.Queue;
import java.util.Iterator;
import java.util.ArrayDeque;

import eta.runtime.RtsFlags;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.Thunk;
import eta.runtime.stg.Closure;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.stg.TSO.WhyBlocked.NotBlocked;
import static eta.runtime.RtsMessages.debugBelch;

public class BlockingQueue extends BlackHole implements Iterable<MessageBlackHole> {
    public final TSO owner;
    public final Thunk bh;
    public final Queue<MessageBlackHole> messages;

    public BlockingQueue(final TSO owner, final MessageBlackHole msg) {
        this.owner = owner;
        this.bh = msg.bh;
        this.messages = new ArrayDeque<MessageBlackHole>();
        messages.offer(msg);
    }

    @Override
    public final Closure enter(StgContext context) {
        barf("BlockingQueue object entered!");
        return null;
    }

    @Override
    public Iterator<MessageBlackHole> iterator() {
        return messages.iterator();
    }

    public final void clear() {
        owner   = null;
        bh      = null;
        message.clear();
    }

}
