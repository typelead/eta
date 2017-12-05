package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Function1 extends Function {
    public int arity() { return 1; }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p);
        context.trampoline = old;
        return result.applyV(context);
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply1(context, p2);
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply1V(context, p2);
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply2(context, p2, p3);
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply2V(context, p2, p3);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply3(context, p2, p3, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply4(context, p2, p3, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply1(context, p1);
        context.trampoline = old;
        return result.apply5(context, p2, p3, p4, p5, p6);
    }
}
