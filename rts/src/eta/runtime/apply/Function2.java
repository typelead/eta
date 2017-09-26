package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Function2 extends Function {
    public int arity() { return 2; }

    @Override
    public Closure apply2V(StgContext context, Closure p1, Closure p2) {
        return apply2(context, p1, p2).applyV(context);
    }

    @Override
    public Closure apply3(StgContext context, Closure p1, Closure p2, Closure p3) {
        return apply2(context, p1, p2).apply1(context, p3);
    }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        return apply2(context, p1, p2).apply1V(context, p3);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        return apply2(context, p1, p2).apply2(context, p3, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        return apply2(context, p1, p2).apply3(context, p3, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        return apply2(context, p1, p2).apply4(context, p3, p4, p5, p6);
    }
}
