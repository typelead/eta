package eta.runtime.stg;

import java.io.Serializable;


import static eta.runtime.RuntimeLogging.barf;

public abstract class Closure implements Serializable {

    public abstract Closure enter(StgContext context);

    public Closure getEvaluated() { return null; }

    @Override
    public String toString() {
        return Print.closureToString(this);
    }

    public Closure evaluate(StgContext context) {
        barf("Cannot evaluate " + this);
        return null;
    }

    public Closure evaluateTail(StgContext context) {
        if (context.trampoline) {
            Stg.evaluateTail(context, this);
            context.firstTime = true;
        }
        return evaluate(context);
    }

    /* Applications */
    public Closure applyV(StgContext context) {
        barf("Cannot apply state token to " + this);
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

    public Closure apply1(StgContext context, Closure p) {
        barf("Cannot apply a closure to " + this);
        return null;
    }

    public Closure apply1V(StgContext context, Closure p) {
        barf("Cannot apply a closure and state token to " + this);
        return null;
    }

    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        barf("Cannot apply 2 closures to " + this);
        return null;
    }

    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        barf("Cannot apply 2 closures and state token to " + this);
        return null;
    }

    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("Cannot apply 3 closures to " + this);
        return null;
    }

    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        barf("Cannot apply 3 closures and state token to " + this);
        return null;
    }

    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        barf("Cannot apply 4 closures to " + this);
        return null;
    }

    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        barf("Cannot apply 5 closures to " + this);
        return null;
    }

    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        barf("Cannot apply 6 closures to " + this);
        return null;
    }
}
