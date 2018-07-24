package eta.runtime.apply;

import java.util.Map;
import java.util.Deque;

import eta.runtime.stg.Print;
import eta.runtime.stg.Value;
import eta.runtime.stg.Closure;

public abstract class PAP extends Value {
    public Function fun;
    public int arity;
    protected PAP(Function fun, int arity) {
        this.fun = fun;
        this.arity = arity;
    }

    public abstract void writeArgs(StringBuilder sb, Object pending,
                                   Map<Object, Boolean> seen, Deque<Object> stack);
}
