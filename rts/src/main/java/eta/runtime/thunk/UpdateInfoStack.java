package eta.runtime.thunk;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;
import static eta.runtime.stg.TSO.WhyBlocked.*;

public class UpdateInfoStack {
    private UpdateInfo top;
    private UpdateInfo bottom;

    /* Manages a set of free UpdateInfos to avoid excess allocation */
    private UpdateInfo free;

    public UpdateInfoStack() {}

    public final UpdateInfo push(Thunk updatee) {
        UpdateInfo ui = acquireUpdateInfo(updatee);
        if (bottom == null) {
            pushBottom(ui);
        } else {
            pushMiddle(ui);
        }
        return ui;
    }

    private final void pushMiddle(UpdateInfo ui) {
        ui.prev  = top;
        ui.next  = null;
        top.next = ui;
        top = ui;
    }

    private final void pushBottom(UpdateInfo ui) {
        bottom = top = ui;
        ui.prev = null;
        ui.next = null;
    }

    private final UpdateInfo acquireUpdateInfo(Thunk updatee) {
        if (free != null) {
            return grabFreeUpdateInfo(updatee);
        } else {
            return new UpdateInfo(updatee);
        }
    }

    private final UpdateInfo grabFreeUpdateInfo(Thunk updatee) {
        UpdateInfo ui = free;
        ui.updatee = updatee;
        free = free.prev;
        return ui;
    }

    public final Thunk pop() {
        UpdateInfo ui = top;
        Thunk res = ui.updatee;
        adjustAfterPop(ui);
        free = ui.reset(free);
        return res;
    }

    private final void adjustAfterPop(UpdateInfo top) {
        top = top.prev;
        if (top == null) {
            bottom = null;
        } else {
            top.next = null;
        }
        this.top = top;
    }

    public final boolean isEmpty() {
        return bottom == null;
    }

    public final UpdateInfo peek() {
        return top;
    }

    public final void clear() {
        top    = null;
        bottom = null;
    }

    public final UpdateInfo markBackwardsFrom(Capability cap, TSO tso) {
        UpdateInfo ui = top;
        UpdateInfo suspend = null;
        while (ui != null && !ui.marked) {
            ui.marked = true;
            Thunk bh = ui.updatee;
            do {
                Closure p = bh.indirectee;
                if (p != null) {
                    if (p instanceof BlockingQueue) {
                        BlockingQueue bq = (BlockingQueue) p;
                        TSO owner = bq.owner;
                        if (owner != tso) {
                            /* Only suspend computations if the owner is unblocked,
                               otherwise we get deadlocks. See #520. */
                            if (owner.whyBlocked == NotBlocked) {
                                suspend = ui;
                            }
                        }
                    } else if (p != tso) {
                        if (p instanceof TSO) {
                            // Same as above.
                            if (((TSO)p).whyBlocked == NotBlocked) {
                                suspend = ui;
                            }
                        }
                    }
                    break;
                } else {
                    if (bh.tryLock()) {
                        bh.setIndirection(tso);
                        break;
                    } else continue;
                }
            } while (true);
            ui = ui.prev;
        }
        return suspend;
    }
}
