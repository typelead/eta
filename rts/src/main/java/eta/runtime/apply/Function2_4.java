package eta.runtime.apply;

import eta.runtime.stg.Closure;

public class Function2_4 extends Function2 {
    public Closure x1;
    public Closure x2;
    public Closure x3;
    public Closure x4;

    public Function2_4(Closure x1, Closure x2, Closure x3, Closure x4) {
        this.x1 = x1;
        this.x2 = x2;
        this.x3 = x3;
        this.x4 = x4;
    }
}
