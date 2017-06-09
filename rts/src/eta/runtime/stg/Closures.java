package eta.runtime.stg;

/* Standard closures used throughout the runtime system. */

public class Closures {

    public static final Closure False_closure            = null;
    public static final Closure flushStdHandles_closure  = null;
    public static final Closure runSparks_closure        = null;
    public static final Closure nonTermination_closure   = null;
    public static final Closure nestedAtomically_closure = null;
    public static final Closure evalLazyIO_closure       = new EvalLazyIO();
    public static final Closure evalIO_closure           = new EvalIO();
    public static final Closure evalJava_closure         = new EvalJava();

    static {
        try {
            flushStdHandles_closure  = loadClosure("base.ghc.TopHandler"
                                                  ,"flushStdHandles");
            False_closure            = loadClosure("ghc_prim.ghc.Types"
                                                  ,"False");
            runSparks_closure        = loadClosure("base.ghc.conc.Sync"
                                                  ,"runSparks");
            nonTermination_closure   = loadClosure("base.control.exception.Base"
                                                  ,"nonTermination");
            nestedAtomically_closure = loadClosure("base.control.exception.Base"
                                                  ,"nestedAtomically");

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
        return (Closure) Class.forName(className).getMethod(closureName + "_closure").invoke(null);
    }

    /* Closure definitions that do the main evaluation. */

    public static class EvalLazyIO extends Closure {
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

    public static class EvalIO extends Closure {
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
