package eta.runtime.stg;

import static eta.runtime.RuntimeLogging.barf;

public abstract class DataCon extends Value {

    @Override
    public Closure enter(StgContext context) { return this; }

    public abstract int getTag();

    public Closure get(int i) {
        barf(this + ": get not implemented!");
        return null;
    }
}
