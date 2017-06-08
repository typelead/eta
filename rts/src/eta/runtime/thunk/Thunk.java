package eta.runtime.thunk;

import java.util.ListIterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import eta.runtime.stg.Stg;
import eta.runtime.stg.Capability;
import eta.runtime.stg.StgTSO;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StackFrame;
import eta.runtime.stg.StgEnter;
import eta.runtime.exception.StgRaise;
import eta.runtime.message.MessageBlackHole;

import static eta.runtime.stg.StgTSO.WhatNext.ThreadRun;
import static eta.runtime.stg.StgTSO.WhyBlocked.BlockedOnBlackHole;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;
import static eta.runtime.RtsMessages.debugBelch;

public class Thunk {
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

    public static synchronized void revertCAFs() {
        for (StgIndStatic c: revertibleCAFList) {
            c.indirectee = null;
        }
        revertibleCAFList.clear();
    }
}
