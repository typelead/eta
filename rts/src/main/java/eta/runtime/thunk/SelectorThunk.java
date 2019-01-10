package eta.runtime.thunk;

import eta.runtime.stg.Closure;
import eta.runtime.stg.DataCon;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.Value;
import eta.runtime.util.CompactReferenceSet;

import static eta.runtime.RuntimeLogging.*;

public class SelectorThunk extends UpdatableThunk {
    private int index;
    public volatile Closure x1;

    private SelectorThunk(final int i, final Closure x1) {
        super();
        this.index = i;
        this.x1 = x1;
    }

    public static SelectorThunk create(final StgContext context,
                                       final int i, final Closure x1) {
        SelectorThunk t = new SelectorThunk(i, x1);
        context.registerSelectorThunk(t);
        return t;
    }

    /* Constants returned by optimize */
    public static final int CLEARED = 1;
    public static final int PRODUCT_NOT_EVALUATED = 2;
    public static final int OPTIMIZED = 4;
    public static final int SELECTOR_ROOT = 8;

    /* The method is safe to run across threads.
       Returns CLEARED, PRODUCT_NOT_EVALUATED, OPTIMIZED, or SELECTOR_ROOT. */
    public int optimize() {

        Closure v = this.x1;

        /* Check if this thunk has been cleared */
        if (v != null) {

            /* Check if first optimization has yet to happen */
            if (this.index >= 0) {

                /* Check if the product type has been evaluated */
                if ((v = v.getEvaluated()) != null) {

                    /* Store the index to make sure it doesn't flip to -1 while we're
                       extracting the field. */
                    final int index = this.index;

                    if (index >= 0) {

                        /* Replace it with the appropriate field */
                        v = ((DataCon) v).get(index);
                        this.index = -1;
                        this.x1    = v;
                        if (v instanceof SelectorThunk) {
                            return SELECTOR_ROOT;
                        }
                    }
                    return OPTIMIZED;
                } else {
                    return PRODUCT_NOT_EVALUATED;
                }
            } else {
                /* Check if pointing to a selector thunk. */
                if (v instanceof SelectorThunk) {
                    return SELECTOR_ROOT;
                } else {
                    return OPTIMIZED;
                }
            }
        } else {
            return CLEARED;
        }
    }

    public void optimizeChain(CompactReferenceSet<SelectorThunk> thunkSet) {
        Closure v = this.x1;
        if (v != null) {
            while (v instanceof SelectorThunk) {
                final SelectorThunk t = (SelectorThunk) v;
                thunkSet.add(t);
                final int opt = t.optimize();
                switch (opt) {
                    case SELECTOR_ROOT:
                        v = t.x1;
                        break;
                    case PRODUCT_NOT_EVALUATED:
                    case OPTIMIZED:
                        this.x1 = v;
                        return;
                    case CLEARED:
                        this.x1 = t.indirectee;
                        return;
                    default:
                        barf("optimizeChain: unexpected result: " + opt);
                }
            }
        }
    }

    @Override
    public final Closure thunkEnter(final StgContext context) {
        final Closure x1 = this.x1;
        final Closure ind = this.indirectee;
        if (ind instanceof Value) return ind;
        else if (this.index >= 0) {
            return ((DataCon) x1.evaluate(context)).get(index).evaluateTail(context);
        } else {
            return x1.evaluateTail(context);
        }
    }

    @Override
    public final void clear() {
        this.x1 = null;
    }
}
