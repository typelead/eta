package ghcvm.runtime.types;

import java.util.Iterator;
import java.util.Queue;

import ghcvm.runtime.closure.StgClosure;
import ghcvm.runtime.closure.StgInd;
import ghcvm.runtime.message.MessageBlackHole;

public class StgBlockingQueue extends StgClosure {
    public StgTSO owner;
    public StgInd blackhole;
    public Queue<MessageBlackHole> messages;
    // Elements of the array

    public StgBlockingQueue() {}

    @Override
    public void thunkUpdate(Capability cap, StgTSO tso) {
        if (owner != tso) {
            cap.checkBlockingQueues(tso);
        } else {
            cap.wakeBlockingQueue(this);
        }
    }

    public Iterator<MessageBlackHole> iterator() {
        return messages.iterator();
    }

}
