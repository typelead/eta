package eta.base;

import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.thunk.CAF;
import eta.base.Utils;

public class StringCAF extends CAF {

    private String s;

    public StringCAF(final String s) {
        this.s = s;
    }

    @Override
    public Closure thunkEnter(StgContext context) {
        return Utils.jstringToString(null, s);
    }

    @Override
    public void clear() {
        this.s = null;
    }
}
