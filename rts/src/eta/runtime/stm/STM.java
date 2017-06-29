package eta.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.RuntimeOptions;
import eta.runtime.stg.Stg;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.Exception;

import static eta.runtime.stm.TransactionRecord.State.TREC_ACTIVE;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;

public class STM {
    /* STM RTS primops */
    public static final Object awake = new Object();

    public static Closure newTVar(StgContext context, Closure init) {
        return new TVar(init);
    }

    public static Closure readTVar(StgContext context, TVar tvar) {
        TransactionRecord trec = context.currentTSO.trec;
        assert trec != null;
        assert trec.state == TREC_ACTIVE || trec.state = TREC_CONDEMNED;
        Closure result;
        EntrySearchResult searchResult = trec.getNested(tvar);
        TransactionRecord entryIn      = null;
        TransactionEntry  entry        = null;
        if (searchResult != null) {
            entryIn = searchResult.header;
            entry   = searchResult.entry;
        }
        if (entry != null) {
            if (entryIn != trec) {
                /* If the entry was found in a parent TRec, copy the entry to
                   the current trec, since you have just read it in the
                   current transaction. */
                trec.put(tvar, entry.expectedValue, entry.newValue);
            }
            result = entry.newValue;
        } else {
            Closure currentValue = tvar.currentValue();
            trec.put(tvar, currentValue, currentValue);
        }
        return result;
    }

    public static Closure readTVarIO(StgContext context, TVar tvar) {
        return tvar.currentValue();
    }

    public static Closure writeTVar(StgContext context, TVar tvar, Closure newValue) {
        TransactionRecord trec         = context.currentTSO.trec;
        EntrySearchResult searchResult = trec.getNested(tvar);
        TransactionRecord entryIn      = null;
        TransactionEntry  entry        = null;
        if (searchResult != null) {
            entryIn = searchResult.header;
            entry   = searchResult.entry;
        }
        if (entry != null) {
            if (entryIn == trec) {
                entry.newValue = newValue;
            } else {
                trec.put(tvar, entry.expectedValue, newValue);
            }
        } else {
            Closure currentValue = tvar.currentValue();
            trec.put(tvar, currentValue, newValue);
        }
        return null;
    }

    public static Closure check(StgContext context, Closure invariant) {
        TransactionRecord trec = context.currentTSO.trec;
        assert trec != null;
        assert trec.state == TREC_ACTIVE || trec.state == TREC_CONDEMNED;
        trec.checkInvariant(invariant);
        return null;
    }


    public static Closure atomically(StgContext context, Closure code) {
        TSO tso = context.currentTSO;
        TransactionRecord outer = tso.trec;
        if (outer != null) {
            return Exception.raise(context, nestedAtomically);
        } else {
            TransactionRecord trec = TransactionRecord.start(outer);
            tso.trec = trec;
            Queue<InvariantCheck> invariants = new LinkedDeque<InvariantCheck>();
            Capability cap = context.currentCapability;
            Closure result;
            Closure frameResult;
            boolean runCode = true;
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
                } catch (EtaException e) {
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
                } catch (EtaAsyncException e) {
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
                } catch (RetryException e) {
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
                        /* If the transaction is invalid, retry. */
                        trec     = TransactionRecord.start(null);
                        tso.trec = trec;
                        runCode  = true;
                        continue;
                    } else {
                        trec     = TransactionRecord.start(outer);
                        tso.trec = trec;
                        runCode  = true;
                        continue;
                    }
                }
            } while (true);
        }
        return null;
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
        } catch (EtaException e) {
            trec  = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            return handler.applyPV(context, e.exception);
        } catch (EtaAsyncException e) {
            trec  = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            throw e;
        } catch (RetryException e) {
            trec  = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            throw e;
        }
        return null;
    }

    public static Closure catchRetry(StgContext context, Closure firstCode, Closure altCode) {
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
                if (valid) {
                    tso.trec = outer;
                    return result;
                } else {
                    trec = TransactionRecord.start(outer);
                    tso.trec = trec;
                    continue;
                }
            } catch (EtaException e) {
                trec  = tso.trec;
                outer = trec.enclosingTrec;
                trec.abort();
                tso.trec = outer;
                throw e;
            } catch (EtaAsyncException e) {
                trec  = tso.trec;
                outer = trec.enclosingTrec;
                trec.abort();
                tso.trec = outer;
                throw e;
            } catch (RetryException e) {
                trec  = tso.trec;
                outer = trec.enclosingTrec;
                assert outer != null;
                trec.abort();
                if (runningAltCode) {
                    tso.trec = outer;
                    throw e;
                } else {
                    trec           = TransactionRecord.start(outer);
                    tso.trec       = trec;
                    runningAltCode = true;
                    continue;
                }
            }
        } while (true);
        return null;
    }

    public static Closure retry(StgContext context) {
        throw RetryException.INSTANCE;
        return null;
    }
}
