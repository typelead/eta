package eta.runtime.apply;

import eta.runtime.stg.Closure;
import eta.runtime.stg.Stg;
import eta.runtime.stg.StgContext;

public class FunctionId extends Function1 {
    public static FunctionId INSTANCE = new FunctionId();
    
    public final Closure apply1(StgContext paramStgContext, Closure paramClosure) {
        if (paramStgContext.trampoline)
            Stg.apply1Tail(paramStgContext, this, paramClosure);
        return paramClosure.evaluate(paramStgContext);
    }
    
    public final Closure enter(StgContext paramStgContext) {
        if (paramStgContext.trampoline)
            Stg.enterTail(paramStgContext, this);
        return paramStgContext.R1.evaluate(paramStgContext);
    }
}
