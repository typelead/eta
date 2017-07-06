package eta.runtime.stg;

import eta.runtime.thunk.Thunk;
import eta.runtime.thunk.UpdateInfo;

public class StgContext {
    public Capability myCapability;
    public TSO currentTSO;
    public ArgumentStack argStack = new ArgumentStack();

    public void reset(Capability cap, TSO t) {
        myCapability = cap;
        currentTSO = t;
        argStack = new ArgumentStack();
    }

    public UpdateInfo pushUpdate(Thunk updatee) {
        return currentTSO.updateInfoStack.push(updatee);
    }

    public Thunk popUpdate() {
        return currentTSO.updateInfoStack.pop();
    }

    public void merge(AbstractArgumentStack argStack) {
        AbstractArgumentStack stack =
            AbstractArgumentStack.Builder.from(argStack)
            .setSimple(false)
            .build();
        // TODO: Need to synchronize this?
        this.argStack = (ArgumentStack) stack;
    }

    public static StgContext acquire() {
        Capability cap = Capability.getLocal();
        StgContext context = cap.context;
        if (context.currentTSO != null) {
            return context;
        } else {
            context.currentTSO = new TSO(null);
            return context;
        }
    }

    public void dump() {
        System.out.println("Context Dump");
        System.out.println("currentTSO: " + currentTSO);
        System.out.println("myCapabilitymyCapability: " + myCapability);
        argStack.dump();
    }

    public Closure R(int index) {
        return argStack.R(index);
    }

    public void R(int index, Closure closure) {
        argStack.R(index, closure);
    }

    public Object O(int index) {
        return argStack.O(index);
    }

    public void O(int index, Object closure) {
        argStack.O(index, closure);
    }

    public int I(int index) {
        return argStack.I(index);
    }

    public void I(int index, int closure) {
        argStack.I(index, closure);
    }

    public long L(int index) {
        return argStack.L(index);
    }

    public void L(int index, long closure) {
        argStack.L(index, closure);
    }

    public float F(int index) {
        return argStack.F(index);
    }

    public void F(int index, float closure) {
        argStack.F(index, closure);
    }

    public double D(int index) {
        return argStack.D(index);
    }

    public void D(int index, double closure) {
        argStack.D(index, closure);
    }
}
