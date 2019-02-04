package eta.runtime.stg;

import java.lang.ref.WeakReference;
import eta.runtime.util.CompactReferenceSet;

import eta.runtime.Runtime;
import eta.runtime.thunk.SelectorThunk;

import static eta.runtime.RuntimeLogging.*;
import static eta.runtime.thunk.SelectorThunk.*;

public class SelectorThunkManager {

    private final int size;
    private boolean initialized = false;
    private int nextIndex;
    private WeakReference<SelectorThunk>[] selectorThunks;
    private CompactReferenceSet<SelectorThunk> thunkSet;

    public SelectorThunkManager(final int size) {
        this.size = size;
    }

    @SuppressWarnings("unchecked")
    private final void init() {
        this.selectorThunks   = (WeakReference<SelectorThunk>[]) new WeakReference[size];
        this.nextIndex        = 0;
        this.thunkSet         = new CompactReferenceSet<SelectorThunk>(size);
        this.initialized      = true;
    }

    public final void reset() {
        this.initialized    = false;
        this.selectorThunks = null;
        this.thunkSet       = null;
    }

    public final void register(SelectorThunk t) {
        if (!initialized) init();
        int next = this.nextIndex++;
        this.selectorThunks[next] = new WeakReference<SelectorThunk>(t);
        if ((next + 1) >= this.selectorThunks.length) {
            optimize();
            if (this.nextIndex >= this.selectorThunks.length) {
                resize();
            }
        }
    }

    private final void optimize() {
        final WeakReference<SelectorThunk>[] selectorThunks = this.selectorThunks;
        final CompactReferenceSet<SelectorThunk> thunkSet = this.thunkSet;
        final int len = this.nextIndex;
        int j = 0;
        for (int i = 0; i < len; i++) {
            final WeakReference<SelectorThunk> ref = selectorThunks[i];
            final SelectorThunk selectorThunk = ref.get();
            boolean keep = false;

            /* Check if GC'd */
            if (selectorThunk != null && !thunkSet.remove(selectorThunk)) {
                switch (selectorThunk.optimize()) {
                    case SELECTOR_ROOT:
                        selectorThunk.optimizeChain(thunkSet);
                        keep = true;
                        break;
                    case PRODUCT_NOT_EVALUATED:
                        keep = true;
                        break;
                }
            }

            if (keep) {
                if (j < i) {
                    /* Shift it back if there's space */
                    selectorThunks[j] = ref;
                    selectorThunks[i] = null;
                }
                j++;
            } else {
                selectorThunks[i] = null;
            }
        }

        thunkSet.clear();

        this.nextIndex = j;

        if (Runtime.debugSelectors()) {
            debugSelectors("Optimized: " + (len - j) + " / " + len);
        }
    }

    private final void resize() {
        final int len = this.selectorThunks.length;

        @SuppressWarnings("unchecked")
        final WeakReference<SelectorThunk>[] newSelectorThunks =
            (WeakReference<SelectorThunk>[]) new WeakReference[len + size];

        System.arraycopy(this.selectorThunks, 0, newSelectorThunks, 0, len);
        this.selectorThunks = newSelectorThunks;
    }
}
