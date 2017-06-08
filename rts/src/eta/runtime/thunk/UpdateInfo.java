package eta.runtime.thunk;

public class UpdateInfo {
    Thunk   updatee;
    boolean    marked;
    UpdateInfo next;
    UpdateInfo prev;

    public UpdateInfo(Thunk updatee, boolean marked) {
        this.updatee = updatee;
        this.marked  = marked;
    }
}
