package eta.runtime.apply;


import eta.runtime.stg.Value;

import static eta.runtime.stg.Print.*;

public abstract class PAP extends Value {
    public Function fun;
    public int arity;
    protected PAP(Function fun, int arity) {
        this.fun = fun;
        this.arity = arity;
    }

    public abstract void writeArgs(Object pending, PrintState ps);
}
