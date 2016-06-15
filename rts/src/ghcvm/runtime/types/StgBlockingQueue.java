package ghcvm.runtime.types;

import java.util.Iterator;
import java.util.Queue;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgInd;
import ghcvm.runtime.message.MessageBlackHole;

public class StgBlockingQueue extends StgClosure implements Iterable<MessageBlackHole> {
    public final StgTSO owner;
    public final StgInd blackhole;
    public final Queue<MessageBlackHole> messages;
    public boolean clean = false;

    public StgBlockingQueue(final StgTSO owner, final MessageBlackHole msg) {
        this.owner = owner;
        this.bh = msg.bh;
        this.messages = new ArrayDeque<MessageBlackHole>();
        messages.offer(msg);
    }

    @Override
    public final boolean isEvaluated() { return false; }

    @Override
    public final boolean blackHole(Capability cap, MessageBlackHole msg) {
        if (messages.isEmpty()) {
            if (owner.cap != cap) {
                cap.sendMessage(owner.cap, msg);
            } else {
                messages.offer(msg);
                clean = false;
                if (owner.whyBlocked == NotBlocked && owner.id != msg.tso.id) {
                    cap.promoteInRunQueue(cap, owner);
                }
            }
            return false;
        } else {
            return true;
        }
    }

    @Override
    public void thunkUpdate(Capability cap, StgTSO tso) {
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

}
