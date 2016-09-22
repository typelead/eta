package ghcvm.runtime.stg;

import java.util.Queue;
import java.util.Iterator;
import java.util.ArrayDeque;

import ghcvm.runtime.stg.StgTSO;
import ghcvm.runtime.stg.Capability;
import ghcvm.runtime.thunk.StgThunk;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.message.MessageBlackHole;
import static ghcvm.runtime.stg.StgTSO.WhyBlocked.NotBlocked;

public class StgBlockingQueue extends StgClosure implements Iterable<MessageBlackHole> {
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
    public final boolean blackHole(Capability cap, MessageBlackHole msg) {
        if (messages.isEmpty()) {
            if (owner.cap != cap) {
                cap.sendMessage(owner.cap, msg);
            } else {
                messages.offer(msg);
                if (owner.whyBlocked == NotBlocked && owner.id != msg.tso.id) {
                    cap.promoteInRunQueue(owner);
                }
            }
            return false;
        } else {
            return true;
        }
    }

    @Override
    public void doUpdateThunk(Capability cap, StgTSO tso) {
        if (owner != tso) {
            cap.checkBlockingQueues(tso);
        } else {
            cap.wakeBlockingQueue(this);
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
