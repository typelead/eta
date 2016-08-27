package ghcvm.runtime.io;

import ghcvm.runtime.stg.RtsFun;
import ghcvm.runtime.stg.StgContext;
import ghcvm.runtime.stg.StgClosure;
import ghcvm.runtime.thunk.Ap2Upd;
import ghcvm.runtime.thunk.SelectorPUpd;
import ghcvm.runtime.RtsFlags;

public class IO {

    public static void _decodeFloat_Int(StgContext context, float f) {
        int bits = Float.floatToRawIntBits(f);
        int s = ((bits >> 31) == 0) ? 1 : -1;
        int e = ((bits >> 23) & 0xff);
        int m = (e == 0) ?
            (bits & 0x7fffff) << 1 :
            (bits & 0x7fffff) | 0x800000;
        context.I(1, s * m);
        context.I(2, e - 150);
    }

    public static RtsFun decodeFloat_Int = new RtsFun() {
        @Override
        public void enter(StgContext context) {
            float f = context.F(1);
            _decodeFloat_Int(context, f);
        }
    };

    public static RtsFun newMutVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgClosure init = context.R(1);
                StgMutVar mv = new StgMutVar(init);
                context.R(1, mv);
            }
        };

    public static RtsFun atomicModifyMutVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMutVar mv = (StgMutVar) context.R(1);
                StgClosure f = context.R(2);
                Ap2Upd z = new Ap2Upd(f, null);
                SelectorPUpd y = new SelectorPUpd(0, z);
                SelectorPUpd r = new SelectorPUpd(1, z);
                do {
                    StgClosure x = mv.value;
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
        };

    public static RtsFun casMutVar = new RtsFun() {
            @Override
            public void enter(StgContext context) {
                StgMutVar mv = (StgMutVar) context.R(1);
                StgClosure old = context.R(2);
                StgClosure new_ = context.R(3);
                if (mv.cas(old, new_)) {
                    context.I(1, 0);
                    context.R(1, new_);
                } else {
                    context.I(1, 1);
                    // TODO: Should there be a valid value here?
                    context.R(1, null);
                }
            }
        };
}
