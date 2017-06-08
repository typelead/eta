package eta.runtime.thunk;

public class UpdateInfo {
    StgThunk   updatee;
    boolean    marked;
    UpdateInfo next;
    UpdateInfo prev;

    public UpdateInfo(StgThunk updatee, boolean marked) {
        this.updatee = updatee;
        this.marked  = marked;
    }
}
