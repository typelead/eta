package eta.runtime.apply;

import eta.runtime.stg.Value;
import eta.runtime.stg.Closure;

public abstract class PAP extends Value {
    public Closure fun;
    public int arity;
    protected PAP(Closure fun, int arity) {
        this.fun = fun;
        this.arity = arity;
    }
}
