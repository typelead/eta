package ghcvm.runtime.types;

import java.util.Queue;
import java.util.Iterator;
import java.util.ArrayDeque;

import ghcvm.runtime.types.StgTSO;
import ghcvm.runtime.types.Capability;
import ghcvm.runtime.thunk.StgInd;
import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.message.MessageBlackHole;
import static ghcvm.runtime.types.StgTSO.WhyBlocked.NotBlocked;

public class StgBlockingQueue extends StgClosure implements Iterable<MessageBlackHole> {
    public final StgTSO owner;
    public final StgInd bh;
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
                    cap.promoteInRunQueue(owner);
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

    public final void clear() {
        messages.clear();
    }

}
