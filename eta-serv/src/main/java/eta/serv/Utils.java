package eta.serv;

import java.util.ArrayList;
import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;
import java.io.StringWriter;
import java.io.PrintWriter;
import java.lang.management.ManagementFactory;

import eta.runtime.Runtime;
import eta.runtime.io.MemoryManager;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.stg.StablePtrTable;

import ghc_prim.ghc.Tuple;
import ghc_prim.ghc.Types;
import ghc_prim.ghc.cstring.datacons.JByteArray;
import ghc_prim.ghc.types.datacons.Ozh;
import ghc_prim.ghc.types.datacons.ZC;
import ghc_prim.ghc.types.datacons.Czh;
import ghc_prim.ghc.types.tycons.ZMZN;
import base.ghc.word.datacons.W64zh;

public class Utils {

    public static Closure mkApp(StgContext context, Closure e1, Closure e2) {
        return wrap(REPLClassLoader.apply(unwrap(e1), unwrap(e2)));
    }

    public static Closure evalStmt(StgContext context, Closure io) {
        List<Object> result = REPLClassLoader.evalStmt(unwrap(io));
        ListIterator<Object> it = result.listIterator(result.size());
        ZMZN next = (ZMZN) Types.DZMZN();
        while (it.hasPrevious()) {
            next = new ZC(wrap(it.previous()), next);
        }
        return next;
    }

    public static Closure evalIO(StgContext context, Closure io) {
        REPLClassLoader.evalIO(unwrap(io));
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    public static Closure evalString(StgContext context, Closure io) {
        return eta.base.Utils.jstringToString(null, REPLClassLoader.evalString(unwrap(io)));

    }

    public static Closure evalStringToString(StgContext context, Closure io, Closure str) {
        StringBuilder sb = new StringBuilder();
        while ((str = Runtime.evaluate(str)) instanceof ZC) {
            ZC next = (ZC) str;
            sb.appendCodePoint(((Czh)(Runtime.evaluate(next.x1))).x1);
            str = next.x2;
        }
        return eta.base.Utils.jstringToString
            (null, REPLClassLoader.evalStringToString(unwrap(io), sb.toString()));
    }

    public static Closure startTH(StgContext context) {
        return wrap(REPLClassLoader.startTH());
    }

    public static Closure runTH(StgContext context, Closure qstate, Closure q, Closure serialized) {
        REPLClassLoader.runTH(unwrap(qstate), unwrap(q), ((JByteArray) serialized).x1);
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    public static Closure runModFinalizerRefs(StgContext context, Closure serialized,
                                              Closure qstate, Closure qactions) {
        List<Long> actions = new LinkedList<Long>();
        while (qactions instanceof ZC) {
            ZC next = (ZC) qactions;
            actions.add(unwrapW64(next.x1));
            qactions = next.x2;
        }
        REPLClassLoader.runModFinalizerRefs
            (((JByteArray) serialized).x1, unwrap(qstate), actions);
        return ghc_prim.ghc.Tuple.DZ0T();
    }

    public static class QueryResult {
        public List<Class<?>> classInfos;
        public List<String> notFounds;

        public QueryResult(final List<Class<?>> classInfos, final List<String> notFounds) {
            this.classInfos = classInfos;
            this.notFounds  = notFounds;
        }
    }

    public static QueryResult getClassInfo(String[] classes) {
        int numClasses = classes.length;
        ArrayList<Class<?>> classInfos = new ArrayList<Class<?>>(numClasses);
        ArrayList<String>   notFound   = new ArrayList<String>(numClasses);
        for (String c: classes) {
            try {
                classInfos.add(REPLClassLoader.queryClass(c));
            } catch (ClassNotFoundException e) {
                notFound.add(c);
            }
        }
        return new QueryResult(classInfos, notFound);
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

    public static long unwrapW64(Closure c) {
        return ((W64zh) c).x1;
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
