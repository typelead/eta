package ghcvm.runtime.types;

#include "Rts.h"

public class Task {
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
