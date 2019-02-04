package eta.runtime.apply;


import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

import static eta.runtime.stg.Print.*;

public class PAP1_1 extends PAP {
    public Closure p;

    public PAP1_1(Function fun, Closure p) {
        super(fun, 1);
        this.p = p;
    }

    @Override
    public Closure applyV(StgContext context) {
        return fun.apply1V(context, p);
    }

    @Override
    public Closure applyN(StgContext context, int n) {
        context.R1 = p;
        context.I1 = n;
        return fun.enter(context);
    }

    @Override
    public Closure applyL(StgContext context, long l) {
        context.R1 = p;
        context.L1 = l;
        return fun.enter(context);
    }

    @Override
    public Closure applyF(StgContext context, float f) {
        context.R1 = p;
        context.F1 = f;
        return fun.enter(context);
    }

    @Override
    public Closure applyD(StgContext context, double d) {
        context.R1 = p;
        context.D1 = d;
        return fun.enter(context);
    }

    @Override
    public Closure applyO(StgContext context, Object o) {
        context.R1 = p;
        context.O1 = o;
        return fun.enter(context);
    }

    @Override
    public Closure apply1(StgContext context, Closure p) {
        return fun.apply2(context, this.p, p);
    }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p);
        context.trampoline = old;
        return result.applyV(context);
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply1(context, p2);
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply1V(context, p2);
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply2(context, p2, p3);
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply2V(context, p2, p3);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply3(context, p2, p3, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply4(context, p2, p3, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        boolean old = context.getAndSetTrampoline();
        Closure result = fun.apply2(context, this.p, p1);
        context.trampoline = old;
        return result.apply5(context, p2, p3, p4, p5, p6);
    }

    @Override
    public void writeArgs(Object pending, PrintState ps) {
        if (pending != null) {
            // If the main pap.fun is pending, the rest should be pending.
            ps.push(PrintObjectField.create(p, "p"));
            ps.push(pending);
        } else {
            maybeAddPendingWithSpace(writeObjectField(p, "p", ps), ps);
        }
    }
}
