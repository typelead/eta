package ghcvm.runtime.io;

import ghcvm.runtime.stg.RtsFun;
import ghcvm.runtime.stg.StgContext;

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
}
