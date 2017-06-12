package eta.runtime.stg;

/* Standard closures used throughout the runtime system. */

public class Closures {
    /* Static closures */

    public static final Closure False            = null;
    public static final Closure flushStdHandles  = null;
    public static final Closure runSparks        = null;
    public static final Closure nonTermination   = null;
    public static final Closure nestedAtomically = null;

    static {
        try {
            flushStdHandles  = loadClosure("base.ghc.TopHandler", "flushStdHandles");
            False            = loadClosure("ghc_prim.ghc.Types", "False");
            runSparks        = loadClosure("base.ghc.conc.Sync", "runSparks");
            nestedAtomically = loadClosure("base.control.exception.Base", "nestedAtomically");

        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("FATAL ERROR: Failed to load base closures.");
            System.exit(1);
        }
    }

    /* TODO:
       Make this convert user writable closure names to the internal representation.

       Example: base:GHC.Conc.Sync.runSparks -> base.ghc.conc.Sync, runSparks
    */
    public static Closure loadClosure(String className, String closureName) {
        return (Closure) Class.forName(className).getMethod(closureName).invoke(null);
    }

    /* Closure definitions that do the main evaluation. */

    public static final Closure evalLazyIO(Closure p) {
        return new EvalLazyIO(p);
    }

    public static final Closure evalIO(Closure p) {
        return new EvalIO(p);
    }

    private static final Closure evalJava(Object thisObj, Closure p) {
        return new EvalJava(thisObj, p);
    }

    private static class EvalLazyIO extends Closure {
        private final Closure p;

        public EvalLazyIO(Closure p) {
            this.p = p;
        }

        @Override
        public Closure enter(StgContext context) {
            Closure result;
            try {
                result = p.evaluate(context).applyV(context);
                tso.whatNext = ThreadComplete;
            } catch (EtaException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            }
            return result;
        }
    }

    private static class EvalIO extends Closure {
        private final Closure p;

        public EvalIO(Closure p) {
            this.p = p;
        }

        @Override
        public Closure enter(StgContext context) {
            Closure result;
            try {
                result = p.evaluate(context).applyV(context).evaluate(context);
                tso.whatNext = ThreadComplete;
            } catch (EtaException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            }
            return result;
        }
    }

    public static class EvalJava extends Closure {
        private final Object  thisObj;
        private final Closure p;

        public EvalJava(Object thisObj, Closure p) {
            this.thisObj = thisObj;
            this.p       = p;
        }

        @Override
        public Closure enter(StgContext context) {
            Closure result;
            try {
                result = p.evaluate(context).applyO(context, thisObj).evaluate(context);
                tso.whatNext = ThreadComplete;
            } catch (EtaException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                tso.whatNext = ThreadKilled;
                result = e.exception;
            }
            return result;
        }
    }
}
