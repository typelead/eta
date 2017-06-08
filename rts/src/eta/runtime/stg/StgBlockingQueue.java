package eta.runtime.stg;

import java.util.Queue;
import java.util.Iterator;
import java.util.ArrayDeque;

import eta.runtime.RtsFlags;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Capability;
import eta.runtime.thunk.StgThunk;
import eta.runtime.stg.Closure;
import eta.runtime.message.MessageBlackHole;
import static eta.runtime.stg.StgTSO.WhyBlocked.NotBlocked;
import static eta.runtime.RtsMessages.debugBelch;

public class StgBlockingQueue extends StgEvaluating implements Iterable<MessageBlackHole> {
    public final StgTSO owner;
    public final StgThunk bh;
    public final Queue<MessageBlackHole> messages;

    public StgBlockingQueue(final StgTSO owner, final MessageBlackHole msg) {
        this.owner = owner;
        this.bh = msg.bh;
        this.messages = new ArrayDeque<MessageBlackHole>();
        messages.offer(msg);
    }

    @Override
    public final Closure enter(StgContext context) {
        barf("StgBlockingQueue object entered!");
        return null;
    }

    @Override
    public final boolean blackHole(StgThunk bh, Capability cap,
                                   MessageBlackHole msg) {
        assert this.bh == bh;
        assert owner != null;

        if (messages.isEmpty()) {
            return true;
        } else {
            if (RtsFlags.ModeFlags.threaded && owner.cap != cap) {
                cap.sendMessage(owner.cap, msg);
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("cap %d: forwarding message to cap %d",
                               cap.no, owner.cap.no);
                }
            } else {
                messages.offer(msg);
                if (RtsFlags.DebugFlags.scheduler) {
                    debugBelch("cap %d: thread %d blocked on thread %d",
                               msg.tso.id, owner.id);
                }
                if (owner.whyBlocked == NotBlocked && owner.id != msg.tso.id) {
                    cap.promoteInRunQueue(owner);
                }
            }
            return false;
        }
    }

    @Override
    public Iterator<MessageBlackHole> iterator() {
        return messages.iterator();
    }

    public final void clear() {
        messages.clear();
    }

}
