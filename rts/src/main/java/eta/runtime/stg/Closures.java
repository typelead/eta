package eta.runtime.stg;

import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import eta.runtime.apply.FunctionId;
import eta.runtime.thunk.ApO;
import eta.runtime.thunk.Ap1Upd;
import eta.runtime.thunk.Ap2Upd;
import eta.runtime.thunk.Ap3Upd;
import eta.runtime.thunk.Ap4Upd;
import eta.runtime.thunk.Ap5Upd;
import eta.runtime.thunk.Ap6Upd;
import eta.runtime.thunk.Ap7Upd;
import eta.runtime.thunk.Ap1VUpd;
import eta.runtime.thunk.Ap2VUpd;
import eta.runtime.thunk.Ap3VUpd;
import eta.runtime.thunk.Ap4VUpd;
import static eta.runtime.stg.TSO.WhatNext.*;

/* - Utilies for working with Closures from the Java side.
   - Standard closures used throughout the runtime system. */

public class Closures {

    /* Standard Closures */

    public static Closure False;
    public static Closure flushStdHandles;
    public static Closure runSparks;
    public static Closure nonTermination;
    public static Closure blockedIndefinitelyOnMVar;
    public static Closure blockedIndefinitelyOnSTM;
    public static Closure nestedAtomically;
    public static Closure runFinalizerBatch;
    public static Closure $fExceptionJException;
    public static Closure showException;
    public static Closure trivialExtendsInstance;
    public static Closure $fClass_Object;
    /* Standard Constructors */
    public static Constructor Int = null;
    public static Constructor JException = null;
    public static Constructor SomeException = null;
    public static Constructor DZCExtends = null;
    
    /* Classes */
    public static Class<?> ZC;
    public static Class<?> ZMZN;
    public static Class<?> Czh;
    public static Field unCzh;
    public static Class<?> Izh;
    public static Field unIzh;
    public static Class<?> Szh;
    public static Field unSzh;
    public static Class<?> Jzh;
    public static Field unJzh;

    static {
        try {
            False             = loadClosure("ghc_prim.ghc.Types", "DFalse");
            flushStdHandles   = loadClosure("base.ghc.TopHandler", "flushStdHandles");
            runSparks         = loadClosure("base.ghc.conc.Sync", "runSparks");
            nonTermination    = loadClosure("base.control.exception.Base", "nonTermination");
            blockedIndefinitelyOnMVar = loadClosure("base.ghc.io.Exception",
                                                    "blockedIndefinitelyOnMVar");
            blockedIndefinitelyOnSTM  = loadClosure("base.ghc.io.Exception",
                                                    "blockedIndefinitelyOnSTM");

            nestedAtomically  = loadClosure("base.control.exception.Base", "nestedAtomically");
            runFinalizerBatch = loadClosure("base.ghc.Weak", "runFinalizzerBatch");
            Int               = loadDataCon("ghc_prim.ghc.Types", "Izh", int.class);
            JException        = loadDataCon("base.java.Exception", "JException", Exception.class);
            SomeException     = loadDataCon("base.ghc.Exception", "SomeException", Closure.class, Closure.class);
            $fExceptionJException = loadClosure("base.java.Exception", "$fException_JException");
            showException         = loadClosure("base.java.Exception", "showException");
            $fClass_Object    = loadClosure("ghc_prim.ghc.Classes", "$fClass_Object");
            DZCExtends        = loadDataCon("ghc_prim.ghc.classes","DZCExtends",
                                            Closure.class, Closure.class,
                                            Closure.class, Closure.class);
            trivialExtendsInstance = loadTrivialExtendsInstance($fClass_Object);
            
            ZC    = Class.forName("ghc_prim.ghc.types.datacons.ZC");
            ZMZN  = Class.forName("ghc_prim.ghc.types.datacons.ZMZN");
            Czh   = Class.forName("ghc_prim.ghc.types.datacons.Czh");
            unCzh = Czh.getField("x1");
            Izh   = Class.forName("ghc_prim.ghc.types.datacons.Izh");
            unIzh = Izh.getField("x1");
            Szh   = Class.forName("integer.ghc.integer.type.datacons.Szh");
            unSzh = Szh.getField("x1");
            Jzh   = Class.forName("integer.ghc.integer.type.datacons.Jzh");
            unJzh = Jzh.getField("x1");

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
        return Class.forName(className.toLowerCase() + ".datacons." + dataConName).getConstructor(types);
    }

    public static Closure loadTrivialExtendsInstance(Closure $fClass_Object)
        throws InstantiationException, IllegalAccessException, InvocationTargetException
    {
        return (Closure) DZCExtends.newInstance($fClass_Object, $fClass_Object,
                                                FunctionId.INSTANCE, FunctionId.INSTANCE);
    }

    public static Closure getTrivialExtendsInstance()
    {
        return trivialExtendsInstance;
    }
    /* Closures for Main Evaluation */

    public static Closure evalLazyIO(Closure p) {
        return new EvalLazyIO(p);
    }

    public static Closure evalIO(Closure p) {
        return new EvalIO(p);
    }

    public static Closure evalStableIO(int stablePtr) {
        return new EvalStableIO(stablePtr);
    }

    public static Closure evalJava(Object thisObj, Closure p) {
        return new EvalJava(thisObj, p);
    }

    public static class EvalLazyIO extends Closure {
        public Closure p;

        public EvalLazyIO(Closure p) {
            this.p = p;
        }

        @Override
        public Closure enter(StgContext context) {
            Closure result = p.evaluate(context).applyV(context);
            TSO tso = context.currentTSO;
            if (tso.whatNext == ThreadRun) {
                tso.whatNext = ThreadComplete;
            }
            return result;
        }
    }

    public static class EvalStableIO extends EvalIO {

        public EvalStableIO(int stablePtr) {
            super(StablePtrTable.getClosure(stablePtr));
        }
    }

    public static class EvalIO extends Closure {
        private final Closure p;

        public EvalIO(Closure p) {
            this.p = p;
        }

        @Override
        public Closure enter(StgContext context) {
            Closure result = p.evaluate(context).applyV(context).evaluate(context);
            TSO tso = context.currentTSO;
            if (tso.whatNext == ThreadRun) {
                tso.whatNext = ThreadComplete;
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
            Closure result = p.evaluate(context).applyO(context, thisObj).evaluate(context);
            TSO tso = context.currentTSO;
            if (tso.whatNext == ThreadRun) {
                tso.whatNext = ThreadComplete;
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

    public static Closure applyV(Closure e) {
        return new Ap1VUpd(e);
    }

    public static Closure applyV(Closure e0, Closure e1) {
        return new Ap2VUpd(e0, e1);
    }

    public static Closure applyV(Closure e0, Closure e1, Closure e2) {
        return new Ap3VUpd(e0, e1, e2);
    }

    public static Closure applyV(Closure e0, Closure e1, Closure e2, Closure e3) {
        return new Ap4VUpd(e0, e1, e2, e3);
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
        String result;
        try {
            showException.apply1(context, exception);
            result = (String) context.O1;
        } catch (Exception e) {
            result = "Exception was thrown in rendering exception of type "
                   + e.getClass();
        }
        return result;
    }
}
