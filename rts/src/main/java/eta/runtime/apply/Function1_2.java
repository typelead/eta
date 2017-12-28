package eta.runtime.apply;

import eta.runtime.stg.Closure;

public class Function1_2 extends Function1 {
    public Closure x1;
    public Closure x2;

    public Function1_2(Closure x1, Closure x2) {
        this.x1 = x1;
        this.x2 = x2;
    }
}
