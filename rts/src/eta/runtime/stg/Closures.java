package eta.runtime.stg;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import eta.runtime.thunk.ApO;
import eta.runtime.thunk.Ap1Upd;
import eta.runtime.thunk.Ap2Upd;
import eta.runtime.thunk.Ap3Upd;
import eta.runtime.thunk.Ap4Upd;
import eta.runtime.thunk.Ap5Upd;
import eta.runtime.thunk.Ap6Upd;
import eta.runtime.thunk.Ap7Upd;
import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;
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
    public static Closure $fExceptionJException;
    public static Closure showException;

    /* Standard Constructors */
    public static Constructor Int = null;
    public static Constructor JException = null;
    public static Constructor SomeException = null;

    static {
        try {
            False             = loadClosure("ghc_prim.ghc.Types", "DFalse");
            flushStdHandles   = loadClosure("base.ghc.TopHandler", "flushStdHandles");
            runSparks         = loadClosure("base.ghc.conc.Sync", "runSparks");
            nonTermination    = loadClosure("base.control.exception.Base", "nonTermination");
            nestedAtomically  = loadClosure("base.control.exception.Base", "nestedAtomically");
            runFinalizerBatch = loadClosure("base.ghc.Weak", "runFinalizzerBatch");
            Int               = loadDataCon("ghc_prim.ghc.Types", "Izh", int.class);
            JException        = loadDataCon("base.java.Exception", "JException", Exception.class);
            SomeException     = loadDataCon("base.ghc.Exception", "SomeException", Closure.class, Closure.class);
            $fExceptionJException = loadClosure("base.java.Exception", "$fExceptionJException");
            showException         = loadClosure("base.java.Exception", "showException");
        } catch (Exception e) {
            System.err.println("FATAL ERROR: Failed to load base closures.");
            e.printStackTrace();
            System.exit(1);
        }
    }

    public static Closure loadClosure(String className, String closureName)
        throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException,
               InvocationTargetException
    {
        return (Closure) Class.forName(className).getMethod(closureName).invoke(null);
    }

    public static Constructor loadDataCon(String className, String dataConName, Class<?>... types)
        throws ClassNotFoundException, IllegalAccessException, NoSuchMethodException
    {
        return Class.forName(className + "$" + dataConName + "D").getConstructor(types);
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
            Closure result = p.evaluate(context).applyV(context);
            context.currentTSO.whatNext = ThreadComplete;
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
            Closure result = p.evaluate(context).applyV(context).evaluate(context);
            context.currentTSO.whatNext = ThreadComplete;
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
            Closure result = p.evaluate(context).applyO(context, thisObj).evaluate(context);
            context.currentTSO.whatNext = ThreadComplete;
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

    /* Constructing algebraic data types. */
    public static Closure mkInt(int i) {
        try {
            return (Closure) Int.newInstance(i);
        } catch (InstantiationException e) {
        } catch (IllegalAccessException e) {
        } catch (InvocationTargetException e) {}
        return null;
    }

    /* TODO: Add utilities for constructing all the primitive types. */

    public static Closure mkSomeException(Exception ex) {
        try {
            return (Closure) SomeException.newInstance($fExceptionJException
                                                      ,JException.newInstance(ex));
        } catch (InstantiationException e) {
        } catch (IllegalAccessException e) {
        } catch (InvocationTargetException e) {}
        return null;
    }

    public static String showException(Closure exception) {
        StgContext context = StgContext.acquire();
        showException.applyP(context, exception);
        String result = (String) context.O(1);
        context.reset(null, null);
        return result;
    }
}
