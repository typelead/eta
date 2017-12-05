package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;

public class Function5 extends Function {
    public int arity() { return 5; }

    @Override
    public Closure apply6(StgContext context, Closure p1, Closure p2, Closure p3, Closure p4, Closure p5, Closure p6) {
        boolean old = context.getAndSetTrampoline();
        Closure result = apply5(context, p1, p2, p3, p4, p5);
        context.trampoline = old;
        return result.apply1(context, p6);
    }
}
