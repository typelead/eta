package eta.runtime.stm;

import java.util.Queue;
import java.util.LinkedList;

import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.Closures;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.Exception;
import eta.runtime.exception.StgException;
import eta.runtime.exception.EtaException;
import eta.runtime.exception.EtaAsyncException;
import eta.runtime.exception.RetryException;

public class STM {
    /* STM RTS primops */
    public static final Object awake = new Object();

    public static TVar newTVar(StgContext context, Closure init) {
        return new TVar(init);
    }

    public static Closure readTVar(StgContext context, TVar tvar) {
        TransactionRecord trec = context.currentTSO.trec;
        assert trec != null;
        return trec.read(tvar);
    }

    public static Closure readTVarIO(StgContext context, TVar tvar) {
        return tvar.currentValue();
    }

    public static void writeTVar(StgContext context, TVar tvar, Closure newValue) {
        TransactionRecord trec = context.currentTSO.trec;
        assert trec != null;
        trec.write(tvar, newValue);
    }

    public static Closure check(StgContext context, Closure invariant) {
        TransactionRecord trec = context.currentTSO.trec;
        assert trec != null;
        trec.checkInvariant(invariant);
        /* TODO: Maybe we should make this ghc_prim.Types.DZ0T()? */
        return null;
    }


    public static Closure atomically(StgContext context, Closure code) {
        TSO tso = context.currentTSO;
        TransactionRecord outer = tso.trec;
        if (outer != null) {
            return Exception.raise(context, Closures.nestedAtomically);
        } else {
            Queue<InvariantCheck> invariants = new LinkedList<InvariantCheck>();
            TransactionRecord trec           = TransactionRecord.start(outer);
            Capability cap                   = context.myCapability;
            Closure result                   = null;
            Closure frameResult              = null;
            boolean runCode                  = true;
            tso.trec                         = trec;
            do {
                try {
                    if (runCode) {
                        result  = code.applyV(context);
                        runCode = false;
                    }
                    trec  = tso.trec;
                    outer = trec.enclosingTrec;
                    if (outer == null) {
                        trec.getInvariantsToCheck(invariants);
                        frameResult = result;
                    } else {
                        tso.trec = outer;
                        invariants.peek().myExecution = trec;
                        trec.abort();
                        invariants.poll();
                        trec = outer;
                    }
                    if (invariants.isEmpty()) {
                        boolean valid = trec.commit(cap);
                        if (valid) {
                            tso.trec = null;
                            return frameResult;
                        } else {
                            trec = TransactionRecord.start(null);
                            tso.trec = trec;
                            invariants.clear();
                            runCode = true;
                            continue;
                        }
                    } else {
                        trec = TransactionRecord.start(trec);
                        tso.trec = trec;
                        result = invariants.peek().invariant.code.applyV(context);
                        continue;
                    }
                } catch (java.lang.Exception e_) {
                    if (e_ instanceof EtaAsyncException) {
                        EtaAsyncException e = (EtaAsyncException) e_;
                        if (e.stopAtAtomically) {
                            assert tso.trec.enclosingTrec == null;
                            tso.trec.condemn();
                            result = null;
                            continue;
                        } else {
                            trec = tso.trec;
                            outer = trec.enclosingTrec;
                            trec.abort();
                            tso.trec = outer;
                            /* TODO: Apparently, we need to replace all thunks with
                               code that eventually retried the atomically
                               transaction.

                               unsafePerformIO/unsafeInterleaveIO break this,
                               so we need to figure out an alternative
                               implementation.
                            */
                            throw e;
                        }
                    } else if (e_ instanceof RetryException) {
                        RetryException e = (RetryException) e_;

                        trec  = tso.trec;
                        outer = trec.enclosingTrec;
                        if (outer != null) {
                            trec.abort();
                            trec     = outer;
                            tso.trec = trec;
                            outer    = trec.enclosingTrec;
                        }
                        assert outer == null;
                        boolean valid = trec.wait(tso);
                        if (valid) {
                            trec.revertOwnership(true);
                            do {
                                cap.blockedLoop();
                                valid = trec.reWait(tso);
                            } while (valid);
                        }
                        /* If the transaction is invalid, retry. */
                        trec     = TransactionRecord.start(null);
                        tso.trec = trec;
                        runCode  = true;
                        continue;
                    } else  {
                        EtaException e = null;
                        if (e_ instanceof EtaException) {
                            e = (EtaException) e_;
                        } else {
                            e = EtaException.fromJavaException(tso, e_);
                        }
                        trec = tso.trec;
                        boolean valid = trec.validateNestOfTransactions();
                        outer = trec.enclosingTrec;
                        trec.abort();
                        if (outer != null) {
                            outer.abort();
                        }
                        tso.trec = null;
                        if (valid) {
                            throw e;
                        } else {
                            trec = TransactionRecord.start(null);
                            tso.trec = trec;
                            runCode = true;
                            continue;
                        }
                    }
                }
            } while (true);
        }
    }

    public static Closure catchSTM(StgContext context, Closure code, Closure handler) {
        TSO tso = context.currentTSO;
        TransactionRecord trec = tso.trec;
        TransactionRecord outer = TransactionRecord.start(trec);
        tso.trec = outer;
        Closure result;
        try {
            do {
                result = code.applyV(context);
                trec   = tso.trec;
                outer  = trec.enclosingTrec;
                boolean committed = trec.commitNested();
                if (committed) {
                    tso.trec = outer;
                    return result;
                } else {
                    outer = TransactionRecord.start(outer);
                    tso.trec = outer;
                    continue;
                }
            } while (true);
        } catch (java.lang.Exception e) {
            boolean handle = true;
            if (e instanceof StgException) {
                if (!(e instanceof EtaException)) {
                    handle = false;
                }
            }
            trec  = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            if (handle) {
                EtaException e_;
                if (e instanceof EtaException) {
                    e_ = (EtaException) e;
                } else {
                    e_ = EtaException.fromJavaException(tso, e);
                }
                Closure ret = handler.apply1V(context, e_.exception);
                tso.resetStack();
                return ret;
            } else {
                throw (RuntimeException) e;
            }
        }
    }

    public static Closure catchRetry(StgContext context, Closure firstCode, Closure altCode) throws java.lang.Exception {
        TSO tso = context.currentTSO;
        TransactionRecord trec = TransactionRecord.start(tso.trec);
        tso.trec = trec;
        Closure result;
        boolean runningAltCode = false;
        TransactionRecord outer;
        do {
            try {
                if (runningAltCode) {
                    result = altCode.applyV(context);
                } else {
                    result = firstCode.applyV(context);
                }
                trec  = tso.trec;
                outer = trec.enclosingTrec;
                boolean committed = trec.commitNested();
                if (committed) {
                    tso.trec = outer;
                    return result;
                } else {
                    trec = TransactionRecord.start(outer);
                    tso.trec = trec;
                    continue;
                }
            } catch (java.lang.Exception e) {
                trec  = tso.trec;
                outer = trec.enclosingTrec;
                trec.abort();
                if (e instanceof RetryException) {
                    assert outer != null;
                    tso.trec = outer;
                    if (runningAltCode) {
                        tso.trec = outer;
                        throw e;
                    } else {
                        trec           = TransactionRecord.start(outer);
                        tso.trec       = trec;
                        runningAltCode = true;
                        continue;
                    }
                } else {
                    if (!(e instanceof StgException)) {
                        e = EtaException.fromJavaException(tso, e);
                    }
                    tso.trec = outer;
                    throw e;
                }
            }
        } while (true);
    }

    public static Closure retry(StgContext context) {
        throw RetryException.INSTANCE;
    }
}
