package eta.runtime.thunk;

public class UpdateInfo {
    Thunk      updatee;
    boolean    marked;
    UpdateInfo next;
    UpdateInfo prev;

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
