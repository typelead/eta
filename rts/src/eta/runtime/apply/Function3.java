package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Function3 extends Function {
    public int arity() { return 3; }

    @Override
    public Closure apply3V(StgContext context, Closure p1, Closure p2, Closure p3) {
        return apply3(context, p1, p2, p3).applyV(context);
    }

    @Override
    public Closure apply4(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4) {
        return apply3(context, p1, p2, p3).apply1(context, p4);
    }

    @Override
    public Closure apply5(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5) {
        return apply3(context, p1, p2, p3).apply2(context, p4, p5);
    }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        return apply3(context, p1, p2, p3).apply3(context, p4, p5, p6);
    }
}
