package eta.runtime.stg;

import java.io.Serializable;

import eta.runtime.stg.Capability;
import eta.runtime.stg.TSO;
import eta.runtime.thunk.Thunk;
import eta.runtime.message.MessageBlackHole;

import static eta.runtime.RuntimeLogging.barf;

public abstract class Closure implements Serializable {

    public abstract Closure enter(StgContext context);

    public Closure getEvaluated() { return null; }

    public Closure evaluate(StgContext context) {
        Closure eval = getEvaluated();
        if (eval == null) {
            if (Thread.interrupted()) {
                context.myCapability.idleLoop(false);
            }
            return enter(context);
        } else {
            return eval;
        }
    }

    /* Applications */
    public Closure applyV(StgContext context) {
        barf("Cannot apply realWorld# to " + this);
        return null;
    }

    public Closure applyN(StgContext context, int n) {
        barf("Cannot apply an int to " + this);
        return null;
    }

    public Closure applyL(StgContext context, long l) {
        barf("Cannot apply a long to " + this);
        return null;
    }

    public Closure applyF(StgContext context, float f) {
        barf("Cannot apply a float to " + this);
        return null;
    }

    public Closure applyD(StgContext context, double d) {
        barf("Cannot apply a double to " + this);
        return null;
    }

    public Closure applyO(StgContext context, Object o) {
        barf("Cannot apply an object to " + this);
        return null;
    }

    public Closure applyP(StgContext context, Closure p) {
        barf("Cannot apply a closure to " + this);
        return null;
    }

    public Closure applyPV(StgContext context, Closure p) {
        barf("Cannot apply a closure and realWorld# to " + this);
        return null;
    }

    public Closure applyPP(StgContext context, Closure p1, Closure p2) {
        barf("Cannot apply 2 closures to " + this);
        return null;
    }

    public Closure applyPPV(StgContext context, Closure p1, Closure p2) {
        barf("Cannot apply 2 closures and realWorld# to " + this);
        return null;
    }

    public Closure applyPPP(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("Cannot apply 3 closures to " + this);
        return null;
    }

    public Closure applyPPPV(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("Cannot apply 3 closures and realWorld# to " + this);
        return null;
    }

    public Closure applyPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        barf("Cannot apply 4 closures to " + this);
        return null;
    }

    public Closure applyPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        barf("Cannot apply 5 closures to " + this);
        return null;
    }

    public Closure applyPPPPPP(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        barf("Cannot apply 6 closures to " + this);
        return null;
    }
}
