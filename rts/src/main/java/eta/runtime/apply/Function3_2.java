package eta.runtime.apply;

import eta.runtime.stg.Closure;

public class Function3_2 extends Function3 {
    public Closure x1;
    public Closure x2;

    public Function3_2(Closure x1, Closure x2) {
        this.x1 = x1;
        this.x2 = x2;
    }
}
