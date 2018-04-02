package eta.runtime.thunk;

import eta.Thunk;

public class UpdateInfo {
    private Thunk updatee;
    private boolean    marked;
    UpdateInfo next;
    UpdateInfo prev;

    public Thunk getUpdatee() {
        return updatee;
    }

    public void setUpdatee(Thunk updatee) {
        this.updatee = updatee;
    }

    public boolean isMarked() {
        return marked;
    }

    public void setMarked(boolean marked) {
        this.marked = marked;
    }

    public UpdateInfo(Thunk updatee) {
        this.updatee = updatee;
    }

    UpdateInfo reset(UpdateInfo free) {
        prev = free;
        next = null;
        updatee = null;
        marked = false;
        return this;
    }
}
