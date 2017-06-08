package eta.runtime.thunk;

public class UpdateInfoStack {
    private UpdateInfo top;
    private UpdateInfo bottom;

    /* Manages a set of free UpdateInfos to avoid excess allocation */
    private UpdateInfo free;

    public UpdateInfoStack() {}

    public UpdateInfo push(StgThunk updatee) {
        return push(updatee, false);
    }

    public UpdateInfo push(StgThunk updatee, boolean marked) {
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
        }
        return ui;
    }

    public StgThunk pop() {
        StgThunk res;
        UpdateInfo ui = top;
        top = top.prev;
        if (top == null) {
            bottom = null;
        } else {
            res = ui.updatee;
            top.next = null;
        }
        ui.prev = free;
        ui.next = null;
        ui.updatee = null;
        ui.marked = false;
        free = ui;
        return res;
    }

    public void isEmpty() {
        return bottom == null;
    }

    public UpdateInfo peek() {
        return top;
    }

    public void clear() {
        top    = null;
        bottom = null;
    }

    public void raiseExceptionAfter(Capability cap, StgTSO tso, Closure raise, UpdateInfo ui) {
        if (ui != null) ui = ui.next;
        else ui = bottom;
        while (ui != null) {
            ui.updatee.updateThunk(cap, tso, raise);
            ui = ui.next;
        }
    }

    public UpdateInfo markBackwardsFrom(Capability cap, StgTSO tso) {
        markBackwardsFrom(cap, tso, null);
    }

    public UpdateInfo markBackwardsFrom(Capability cap, StgTSO tso, UpdateInfo ui) {
        if (ui == null) ui = top;
        UpdateInfo suspend;
        while (ui != null && !ui.marked) {
            ui.marked = true;
            StgThunk bh = ui.updatee;
            do {
                Closure p = bh.indirectee;
                if (p != null  && p != tso) {
                    suspend = ui;
                } else {
                    if (Capability.nCapabilities > 1) {
                        synchronized (bh) {
                            p = bh.indirectee;
                            if (p != null) {
                                bh.updateWithIndirection(tso);
                            } else continue;
                        }
                    } else {
                        bh.updateWithIndirection(tso);
                    }
                }
            } while (false);
            ui = ui.prev;
        }
    }
}
