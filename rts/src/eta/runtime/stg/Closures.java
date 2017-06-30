package eta.runtime.stg;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;
import eta.runtime.thunk.ApO;
import eta.runtime.thunk.Ap1Upd;
import eta.runtime.thunk.Ap2Upd;
import eta.runtime.thunk.Ap3Upd;
import eta.runtime.thunk.Ap4Upd;
import eta.runtime.thunk.Ap5Upd;
import eta.runtime.thunk.Ap6Upd;
import eta.runtime.thunk.Ap7Upd;
import static eta.runtime.stg.TSO.WhatNext.*;

/* - Utilies for working with Closures from the Java side.
   - Standard closures used throughout the runtime system. */

public class Closures {

    /* Standard Closures */

    public static Closure False;
    public static Closure flushStdHandles;
    public static Closure runSparks;
    public static Closure nonTermination;
    public static Closure nestedAtomically;
    public static Closure runFinalizerBatch;

    /* Standard Constructors */
    public static Constructor Int = null;

    static {
        try {
            False             = loadClosure("ghc_prim.ghc.Types", "False");
            flushStdHandles   = loadClosure("base.ghc.TopHandler", "flushStdHandles");
            runSparks         = loadClosure("base.ghc.conc.Sync", "runSparks");
            nonTermination    = loadClosure("base.control.exception.Base", "nonTermination");
            nestedAtomically  = loadClosure("base.control.exception.Base", "nestedAtomically");
            runFinalizerBatch = loadClosure("base.ghc.Weak", "runFinalizerBatch");
            Int               = Class.forName("ghc_prim.ghc.Types$IzhD")
                                     .getConstructor(int.class);
        } catch (Exception e) {
            System.err.println("FATAL ERROR: Failed to load base closures.");
            e.printStackTrace();
            System.exit(1);
        }
    }

    /* TODO:
       Make this convert user writable closure names to the internal representation.

       Example: base:GHC.Conc.Sync.runSparks -> base.ghc.conc.Sync, runSparks
    */
    public static Closure loadClosure(String className, String closureName)
        throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException,
               InvocationTargetException
    {
        return (Closure) Class.forName(className).getMethod(closureName).invoke(null);
    }


    /* Closures for Main Evaluation */

    public static Closure evalLazyIO(Closure p) {
        return new EvalLazyIO(p);
    }

    public static Closure evalIO(Closure p) {
        return new EvalIO(p);
    }

    public static Closure evalJava(Object thisObj, Closure p) {
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
                context.currentTSO.whatNext = ThreadComplete;
            } catch (EtaException e) {
                context.currentTSO.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                context.currentTSO.whatNext = ThreadKilled;
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
                context.currentTSO.whatNext = ThreadComplete;
            } catch (EtaException e) {
                context.currentTSO.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                context.currentTSO.whatNext = ThreadKilled;
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
                context.currentTSO.whatNext = ThreadComplete;
            } catch (EtaException e) {
                context.currentTSO.whatNext = ThreadKilled;
                result = e.exception;
            } catch (EtaAsyncException e) {
                context.currentTSO.whatNext = ThreadKilled;
                result = e.exception;
            }
            return result;
        }
    }

    /* Closure Utilities */

    public static Closure force(Closure e) {
        return new Ap1Upd(e);
    }

    public static Closure apply(Closure e0, Closure e1) {
        return new Ap2Upd(e0, e1);
    }

    public static Closure apply(Closure e0, Closure e1, Closure e2) {
        return new Ap3Upd(e0, e1, e2);
    }

    public static Closure apply(Closure e0, Closure e1, Closure e2, Closure e3) {
        return new Ap4Upd(e0, e1, e2, e3);
    }

    public static Closure apply(Closure e0, Closure e1, Closure e2, Closure e3, Closure e4) {
        return new Ap5Upd(e0, e1, e2, e3, e4);
    }

    public static Closure apply(Closure e0, Closure e1, Closure e2, Closure e3, Closure e4, Closure e5) {
        return new Ap6Upd(e0, e1, e2, e3, e4, e5);
    }

    public static Closure apply(Closure e0, Closure e1, Closure e2, Closure e3, Closure e4, Closure e5, Closure e6) {
        return new Ap7Upd(e0, e1, e2, e3, e4, e5, e6);
    }

    public static Closure applyObject(Closure e, Object o) {
        return new ApO(e, o);
    }

    public static Closure mkInt(int i) {
        try {
            return (Closure) Int.newInstance(i);
        } catch (InstantiationException e) {
        } catch (IllegalAccessException e) {
        } catch (InvocationTargetException e) {}
        return null;
    }

    /* TODO: Add utilities for constructing all the primitive types. */
}
