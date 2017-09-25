package eta.runtime.thunk;

import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.TSO;

public class UpdateInfoStack {
    private UpdateInfo top;
    private UpdateInfo bottom;

    /* Manages a set of free UpdateInfos to avoid excess allocation */
    private UpdateInfo free;

    public UpdateInfoStack() {}

    public UpdateInfo push(Thunk updatee) {
        return push(updatee, false);
    }

    public UpdateInfo push(Thunk updatee, boolean marked) {
        UpdateInfo ui;
        if (free != null) {
            ui = free;
            free = free.prev;
            ui.updatee = updatee;
        } else {
            ui = new UpdateInfo(updatee, marked);
        }
        if (bottom == null) {
            bottom = top = ui;
            ui.prev = null;
            ui.next = null;
        } else {
            ui.prev  = top;
            ui.next  = null;
            top.next = ui;
            top = ui;
        }
        return ui;
    }

    public Thunk pop() {
        Thunk res = null;
        UpdateInfo ui = top;
        top = top.prev;
        res = ui.updatee;
        if (top == null) {
            bottom = null;
        } else {
            top.next = null;
        }
        ui.prev = free;
        ui.next = null;
        ui.updatee = null;
        ui.marked = false;
        free = ui;
        return res;
    }

    public boolean isEmpty() {
        return bottom == null;
    }

    public UpdateInfo peek() {
        return top;
    }

    public void clear() {
        top    = null;
        bottom = null;
    }

    public void raiseExceptionAfter(Capability cap, TSO tso, Closure raise, UpdateInfo ui) {
        top = ui;
        if (ui != null) ui = ui.next;
        else ui = bottom;
        if (top != null) {
            top.next = null;
        } else {
            bottom = null;
        }
        while (ui != null) {
            ui.updatee.updateThunk(cap, tso, raise);
            ui = ui.next;
        }
    }

    public UpdateInfo markBackwardsFrom(Capability cap, TSO tso) {
        return markBackwardsFrom(cap, tso, null);
    }

    public UpdateInfo markBackwardsFrom(Capability cap, TSO tso, UpdateInfo ui) {
        if (ui == null) ui = top;
        UpdateInfo suspend = null;
        while (ui != null && !ui.marked) {
            ui.marked = true;
            Thunk bh = ui.updatee;
            do {
                Closure p = bh.indirectee;
                if (p != null) {
                    if (p != tso) {
                        suspend = ui;
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
