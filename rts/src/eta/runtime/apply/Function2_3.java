package eta.runtime.apply;

import eta.runtime.stg.Closure;

public class Function2_3 extends Function2 {
    public Closure x1;
    public Closure x2;
    public Closure x3;

    public Function2_3(Closure x1, Closure x2, Closure x3) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }
}
