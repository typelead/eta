package ghcvm.runtime.types;

#include "Rts.h"

public class Task {
    public Capability cap;
    public InCall incall;
    public int nSpareIncalls;
    public InCall spareIncalls;
    boolean worker;
    boolean stopped;
    boolean runningFinalizers;

    Task next;

    Task allNext;
    Task allPrev;

    public static class InCall {
        public StgTSO tso;
        public StgTSO suspendedTso;
        public Capability suspendedCap;
        public SchedulerStatus returnStatus;
        public REF_CLOSURE_PTR returnValue;
        public Task task;
        public InCall prevStack;
        public InCall prev;
        public InCall next;
    }
}
