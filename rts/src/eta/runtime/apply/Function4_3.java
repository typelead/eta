package eta.runtime.apply;

import eta.runtime.stg.Closure;

public class Function4_3 extends Function4 {
    public Closure x1;
    public Closure x2;
    public Closure x3;

    public Function4_3(Closure x1, Closure x2, Closure x3) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
    }
}
