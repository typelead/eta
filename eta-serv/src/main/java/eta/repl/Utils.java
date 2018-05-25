package eta.repl;

import java.util.List;
import java.util.ListIterator;
import java.nio.ByteBuffer;
import java.io.StringWriter;
import java.io.PrintWriter;
import eta.runtime.io.MemoryManager;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StablePtrTable;
import ghc_prim.ghc.Tuple;
import ghc_prim.ghc.Types;
import ghc_prim.ghc.types.datacons.Ozh;
import ghc_prim.ghc.types.datacons.ZC;
import ghc_prim.ghc.types.tycons.ZMZN;

public class Utils {
    public static ByteBuffer byteStringToByteBuffer(long address, int size) {
        ByteBuffer buf = MemoryManager.getBoundedBuffer(address);
        buf.limit(buf.position() + size);
        return buf;
    }

    public static Closure mkApp(StgContext context, Closure e1, Closure e2) {
        Object e1_ = ((Ozh) e1).x1;
        Object e2_ = ((Ozh) e2).x1;
        return new Ozh(REPLClassLoader.apply(e1_, e2_));
    }

    public static Closure evalStmt(StgContext context, Closure io) {
        List<Object> result = REPLClassLoader.evalStmt(((Ozh) io).x1);
        ListIterator<Object> it = result.listIterator(result.size());
        ZMZN next = (ZMZN) Types.DZMZN();
        while (it.hasPrevious()) {
            next = new ZC(new Ozh(it.previous()), next);
        }
        return next;
    }

    public static Closure evalIO(StgContext context, Closure io) {
        REPLClassLoader.evalIO(((Ozh) io).x1);
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    public static String exceptionToString(Exception e) {
        StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        return sw.toString();
    }

    public static Object getClosure(int index) {
        return ((Ozh) StablePtrTable.getClosure(index)).x1;
    }

    public static void bytesToPtr(byte[] bytes, long address) {
        MemoryManager.getBoundedBuffer(address).put(bytes);
    }
}
