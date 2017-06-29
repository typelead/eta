package eta.runtime.thunk;

public abstract class WhiteHole extends BlackHole {
    public static WhiteHole closure = new WhiteHole();
}
