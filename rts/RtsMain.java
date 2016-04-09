package ghcvm.runtime;

import ghcvm.runtime.SchedulerStatus;
public class RtsMain {
    public static int hsMain(String[] args, CLOSURE_PTR mainClosure, RtsConfig config) {
        int exitStatus;
        SchedulerStatus status = null;

        RtsStartup.hsInitGhcVm(args, config);

        Capability cap = Rts.lock();
        // TODO: Check the values if they are accurate
        Rts.evalLazyIO(cap, mainClosure, 0);
        return 0;
    }
}
