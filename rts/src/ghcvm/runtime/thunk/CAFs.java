package ghcvm.runtime.thunk;

import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import ghcvm.runtime.closure.*;

public class CAFs {
    public static Queue<StgIndStatic> revertibleCAFList = new ConcurrentLinkedQueue<StgIndStatic>();
    public static Queue<StgIndStatic> dynamicCAFList = new ConcurrentLinkedQueue<StgIndStatic>();
    public static Queue<StgIndStatic> debugCAFList = new ConcurrentLinkedQueue<StgIndStatic>();

    private static boolean keepCAFs;

    public static void setKeepCAFs() {
        keepCAFs = true;
    }

    public static boolean shouldKeepCAFs() {
        return keepCAFs;
    }
    public static void revertCAFs() {
        for (StgIndStatic c: revertibleCAFList) {
            c.indirectee = null;
            // code to revert the CAF here
        }
        revertibleCAFList.clear();
    }
}
