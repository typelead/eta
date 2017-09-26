package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Function1 extends Function {
    public int arity() { return 1; }

    @Override
    public Closure apply1V(StgContext context, Closure p) {
        return apply1(context, p).applyV(context);
    }

    @Override
    public Closure apply2(StgContext context, Closure p1, Closure p2) {
        return apply1(context, p1).apply1(context, p2);
    }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        return apply1(context, p1).apply1V(context, p2);
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        return apply1(context, p1).apply2(context, p2, p3);
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        return apply1(context, p1).apply2V(context, p2, p3);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        return apply1(context, p1).apply3(context, p2, p3, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        return apply1(context, p1).apply4(context, p2, p3, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        return apply1(context, p1).apply5(context, p2, p3, p4, p5, p6);
    }
}
