package eta.runtime.io;

import eta.runtime.stg.StgContext;
import eta.runtime.stg.Closure;
import eta.runtime.thunk.Ap2Upd;
import eta.runtime.thunk.SelectorPUpd;
import eta.runtime.RtsFlags;

public class IO {

    public static void decodeFloat_Int(StgContext context, float f) {
        int bits = Float.floatToRawIntBits(f);
        int s = ((bits >> 31) == 0) ? 1 : -1;
        int e = ((bits >> 23) & 0xff);
        int m = (e == 0) ?
            (bits & 0x7fffff) << 1 :
            (bits & 0x7fffff) | 0x800000;
        context.I(1, s * m);
        context.I(2, e - 150);
    }

    public static void atomicModifyMutVar(StgContext context, MutVar mv, Closure f) {
        Ap2Upd z = new Ap2Upd(f, null);
        SelectorPUpd y = new SelectorPUpd(1, z);
        SelectorPUpd r = new SelectorPUpd(2, z);
        do {
            Closure x = mv.value;
            z.p2 = x;
            if (RtsFlags.ModeFlags.threaded) {
                if (!mv.cas(x, y)) {
                    continue;
                }
            } else {
                mv.value = y;
            }
            break;
        } while (true);
        context.R(1, r);
    }

    public static void casMutVar(StgContext context, MutVar mv, Closure old, Closure new_) {
        if (mv.cas(old, new_)) {
            context.I(1, 0);
            context.R(1, new_);
        } else {
            context.I(1, 1);
            context.R(1, null);
        }
    }
}
