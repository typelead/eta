package eta.serv;

import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;

import eta.runtime.io.MemoryManager;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StablePtrTable;

import ghc_prim.ghc.Tuple;
import ghc_prim.ghc.Types;
import ghc_prim.ghc.cstring.datacons.JByteArray;
import ghc_prim.ghc.types.datacons.Ozh;
import ghc_prim.ghc.types.datacons.ZC;
import ghc_prim.ghc.types.tycons.ZMZN;

public class Utils {

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

    public static Closure startTH(StgContext context) {
        return wrap(REPLClassLoader.startTH());
    }

    public static Closure runTH(StgContext context, Closure qstate, Closure q, Closure serialized) {
        REPLClassLoader.runTH(unwrap(qstate), unwrap(q), ((JByteArray) serialized).x1);
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    @SuppressWarnings("unchecked")
    public static Closure runModFinalizerRefs(StgContext context, Closure serialized,
                                              Closure qstate, Closure qactions) {
        List<Object> actions = new LinkedList<Object>();
        while (qactions instanceof ZC) {
            ZC next = (ZC) qactions;
            actions.add(unwrap(next.x1));
            qactions = next.x2;
        }
        REPLClassLoader.runModFinalizerRefs
            (((JByteArray) serialized).x1, unwrap(qstate), actions);
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    public static String exceptionToString(Exception e) {
        StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        return sw.toString();
    }

    public static byte[] unwrapByteArray(Closure c) {
        return ((JByteArray) c).x1;
    }

    public static Object unwrap(Closure c) {
        return ((Ozh) c).x1;
    }

    public static Closure wrap(Object o) {
        return new Ozh(o);
    }

    public static Object getClosure(int index) {
        return ((Ozh) StablePtrTable.getClosure(index)).x1;
    }

    public static long getHeapMemoryUsage() {
        return ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
    }

    public static long getNativeMemoryUsage() {
        return ManagementFactory.getMemoryMXBean().getNonHeapMemoryUsage().getUsed();
    }
}
