package eta.runtime.stm;

import java.util.Stack;
import java.util.ListIterator;
import java.util.concurrent.atomic.AtomicBoolean;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Stg;
import eta.runtime.stg.TSO;
import eta.runtime.stg.Capability;
import eta.runtime.stg.Closure;
import eta.runtime.stg.StgContext;
import eta.runtime.exception.StgException;

import static eta.runtime.stm.TRecState.TREC_ACTIVE;
import static eta.runtime.stg.TSO.WhatNext.ThreadRun;
import static eta.runtime.stg.StgContext.ReturnCode.ThreadBlocked;

public class STM {
    /* STM RTS primops */

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


    public static void atomically(StgContext context, Closure code) {
        TSO tso = context.currentTSO;
        TransactionRecord outer = tso.trec;
        if (outer != null) {
            return StgException.raise(context, nestedAtomically_closure);
        } else {
            TransactionRecord trec = startTransaction(outer);
            tso.trec = trec;
            Queue<InvariantCheck> invariants = new LinkedDeque<InvariantCheck>();
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
                        boolean valid = commitTransaction(trec);
                        if (valid) {
                            tso.trec = null;
                            return frameResult;
                        } else {
                            trec = startTransaction(null);
                            tso.trec = trec;
                            invariants.clear();
                            runCode = true;
                            continue;
                        }
                    } else {
                        trec = startTransaction(trec);
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
                        trec = startTransaction(null);
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
                            LockSupport.park();
                            /* TODO: Should we run the blockedLoop on spurious
                                     wakeups too? Either way, we should consume
                                     the interrupted flag, otherwise future
                                     interrupts won't work. */
                            if (Thread.interrupted()) {
                                context.myCapability.blockedLoop(tso);
                            }
                            valid = trec.reWait(tso);
                            if (!valid) {
                                trec     = startTransaction(null);
                                tso.trec = trec;
                                runCode  = true;
                                break;
                            }
                        } while (valid);
                        continue;
                    } else {
                        trec     = startTransaction(outer);
                        tso.trec = trec;
                        runCode  = true;
                        continue;
                    }
                }
            } while (true);
        }
        return null;
    }

    public static void catchSTM(StgContext context, Closure code, Closure handler) {
        TSO tso = context.currentTSO;
        TransactionRecord trec = tso.trec;
        TransactionRecord outer = startTransaction(curTrec);
        tso.trec = outer;
        Closure result;
        try {
            do {
                result = code.applyV(context);
                trec = tso.trec;
                outer = trec.enclosingTrec;
                boolean committed = commitNestedTransaction(trec);
                if (committed) {
                    tso.trec = outer;
                    return result;
                } else {
                    outer = startTransaction(outer);
                    tso.trec = outer;
                    continue;
                }
            } while (true);
        } catch (EtaException e) {
            trec = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            return handler.applyPV(context, e.exception);
        } catch (EtaAsyncException e) {
            trec = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            throw e;
        } catch (RetryException e) {
            trec = tso.trec;
            outer = trec.enclosingTrec;
            trec.abort();
            tso.trec = outer;
            throw e;
        }
        return null;
    }

    public static void catchRetry(StgContext context, Closure firstCode, Closure altCode) {
        TSO tso = context.currentTSO;
        TransactionRecord trec = startTransaction(tso.trec);
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
                trec = tso.trec;
                outer = trec.enclosingTrec;
                boolean committed = commitNestedTransaction(trec);
                if (valid) {
                    tso.trec = outer;
                    return result;
                } else {
                    trec = startTransaction(outer);
                    tso.trec = trec;
                    continue;
                }
            } catch (EtaException e) {
                trec = tso.trec;
                outer = trec.enclosingTrec;
                trec.abort();
                tso.trec = outer;
                throw e;
            } catch (EtaAsyncException e) {
                trec = tso.trec;
                outer = trec.enclosingTrec;
                trec.abort();
                tso.trec = outer;
                throw e;
            } catch (RetryException e) {
                trec = tso.trec;
                outer = trec.enclosingTrec;
                assert outer != null;
                trec.abort();
                if (runningAltCode) {
                    tso.trec = outer;
                    throw e;
                } else {
                    trec = startTransaction(outer);
                    tso.trec = trec;
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

    /* STM Helper Functions */

    public static TransactionRecord startTransaction(TransactionRecord outer) {
        /* TODO: Handle transaction tokens later if necessary. */
        return new TransactionRecord(outer);
    }

    public static void mergeUpdateInto(TransactionRecord t, TVar tvar, Closure expectedValue, Closure newValue) {
        boolean found = false;
        ListIterator<StgTRecChunk> cit = t.chunkIterator();
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TransactionEntry e: chunk.entries) {
                TVar s = e.tvar;
                if (s == tvar) {
                    found = true;
                    if (e.expectedValue != expectedValue) {
                        t.state = TREC_CONDEMNED;
                    }
                    e.newValue = newValue;
                    break loop;
                }
            }
        }

        if (!found) {
            TransactionEntry ne = getNewEntry(t);
            ne.tvar = tvar;
            ne.expectedValue = expectedValue;
            ne.newValue = newValue;
        }
    }

    public static void removeWatchQueueEntriesForTrec(TransactionRecord trec) {
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();;
        loop:
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TransactionEntry e: chunk.entries) {
                TVar s = e.tvar;
                Closure saw = s.lock(trec);
                s.watchQueue.remove(e.newValue); // TODO: Is this valid?
                s.unlock(trec, saw, false);
            }
        }
    }

    public static void mergeReadInto(TransactionRecord trec, TVar tvar, Closure expectedValue) {
        boolean found = false;
        while (!found && trec != null) {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TransactionEntry e: chunk.entries) {
                    if (e.tvar == tvar) {
                        found = true;
                        if (e.expectedValue != expectedValue) {
                            trec.state = TREC_CONDEMNED;
                        }
                        break loop;
                    }
                }
            }
            trec = trec.enclosingTrec;
        }

        if (!found) {
            TransactionEntry ne     = getNewEntry(trec);
            ne.tvar          = tvar;
            ne.expectedValue = expectedValue;
            ne.newValue      = expectedValue;
        }
    }

    public static boolean commitTransaction(TransactionRecord trec) {
        long maxCommitsAtStart = STM.maxCommits;
        boolean touchedInvariants = !trec.invariantsToCheck.isEmpty();
        if (touchedInvariants) {
            for (InvariantCheck q: trec.invariantsToCheck) {
                AtomicInvariant inv = q.invariant;
                if (!inv.lock()) {
                    trec.state = TREC_CONDEMNED;
                    break;
                }
                TransactionRecord invOldTrec = inv.lastExecution;
                if (invOldTrec != null) {
                    ListIterator<StgTRecChunk> cit = invOldTrec.chunkIterator();
                    while (cit.hasPrevious()) {
                        StgTRecChunk chunk = cit.previous();
                        for (TransactionEntry e: chunk.entries) {
                            mergeReadInto(trec, e.tvar, e.expectedValue);
                        }
                    }
                }
            }
        }

        boolean useReadPhase = STM.configUseReadPhase && !touchedInvariants;
        boolean result = validateAndAcquireOwnership(trec, !useReadPhase, true);
        if (result) {
            if (useReadPhase) {
                result = trec.checkReadOnly();
                long maxCommitsAtEnd = STM.maxCommits;
                long maxConcurrentCommits = (maxCommitsAtEnd - maxCommitsAtStart) + nCapabilities * STM.TOKEN_BATCH_SIZE;
                if ((maxConcurrentCommits >> 32) > 0 || STM.shake()) {
                    result = false;
                }
            }

            if (result) {
                if (touchedInvariants) {
                    for (InvariantCheck q: trec.invariantsToCheck) {
                        AtomicInvariant inv = q.invariant;
                        if (inv.lastExecution != null) {
                            inv.disconnect();
                        }
                        q.myExecution.connectInvariant(inv);
                        inv.unlock();
                    }
                }

                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TransactionEntry e: chunk.entries) {
                        TVar s = e.tvar;
                        if (!useReadPhase || e.newValue != e.expectedValue) {
                            unparkWaitersOn(s);
                            if (RtsFlags.STM.fineGrained) {
                                s.numUpdates++;
                            }
                            s.unlock(trec, e.newValue, true);
                        }
                    }
                }
            } else {
                revertOwnership(trec, false);
            }
        }
        return result;
    }

    public static void condemnTransaction(TransactionRecord trec) {
        if (trec.state == TREC_WAITING) {
            removeWatchQueueEntriesForTrec(trec);
        }
        trec.state = TREC_CONDEMNED;
    }

    public static void unparkWaitersOn(TVar s) {
        Iterator<Closure> iterator = s.watchQueue.descendingIterator();
        while (iterator.hasNext()) {
            Closure tso = iterator.next();
            if (STM.watcherIsTSO(tso)) {
                unparkTSO((TSO) tso);
            }
        }
    }

    public static void unparkTSO(TSO tso) {
        tso.lock();
        if (tso.whyBlocked == BlockedOnSTM &&
            tso.blockInfo == STMAwoken.closure) {
            // trace woken up

        } else if (tso.whyBlocked == BlockedOnSTM) {
            tso.blockInfo = STMAwoken.closure;
            tryWakeupThread(tso);
        } else {
            // trace
        }
        tso.unlock();
    }

    public static boolean reWait(TSO tso) {
        TransactionRecord trec = tso.trec;
        STM.lock(trec);
        boolean result = validateAndAcquireOwnership(trec, true, true);
        if (result) {
            tso.park();
            revertOwnership(trec, true);
        } else {
            if (trec.state != TREC_CONDEMNED) {
                removeWatchQueueEntriesForTrec(trec);
            }
            freeTRecHeader(trec);
        }
        STM.unlock(trec);
        return result;
    }

    public static boolean wait(TSO tso, TransactionRecord trec) {
        STM.lock(trec);
        boolean result = validateAndAcquireOwnership(trec, true, true);
        if (result) {
            buildWatchQueueEntriesForTrec(tso, trec);
            tso.park();
            trec.state = TREC_WAITING;
        } else {
            STM.unlock(trec);
            freeTRecHeader(trec);
        }
        return result;
    }

    public static void buildWatchQueueEntriesForTrec(TSO tso, TransactionRecord trec) {
        ListIterator<StgTRecChunk> cit = trec.chunkIterator();
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TransactionEntry e: chunk.entries) {
                TVar s = e.tvar;
                /* TODO: Fix order of queue */
                s.watchQueue.offer(tso);
                /* NOTE: The original implementation sets a watchqueue
                   closure */
                e.newValue = tso;
            }
        }
    }

    public static void waitUnlock(TransactionRecord trec) {
        revertOwnership(trec, true);
        STM.unlock(trec);
    }

    public static boolean validateNestOfTransactions(TransactionRecord trec) {
        STM.lock(trec);
        TransactionRecord t = trec;
        boolean result = true;
        while (t != null) {
            result = result && validateAndAcquireOwnership(t, true, false);
            t = t.enclosingTrec;
        }

        if (!result && trec.state != TREC_WAITING) {
            trec.state = TREC_CONDEMNED;
        }
        STM.unlock(trec);
        return result;
    }

    public static boolean watcherIsInvariant(Closure c) {
        return c.getClass() == AtomicInvariant.class;
    }

    public static boolean watcherIsTSO(Closure c) {
        return c.getClass() == TSO.class;
    }

    public static class EntrySearchResult {
        public final TransactionRecord header;
        public final TransactionEntry entry;
        public EntrySearchResult(final TransactionRecord header, final TransactionEntry entry) {
            this.header = header;
            this.entry = entry;
        }
    }

    public static EntrySearchResult getEntry(TransactionRecord trec, TVar tvar) {
        EntrySearchResult result = null;
        do {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TransactionEntry entry: chunk.entries) {
                    if (entry.tvar == tvar) {
                        result = new EntrySearchResult(trec, entry);
                        break loop;
                    }
                }
            }
            trec = trec.enclosingTrec;
        } while (result == null && trec != null);
        return result;
    }

    public final InvariantCheck newInvariantCheck(AtomicInvariant invariant) {
        return new InvariantCheck(invariant);
    }

    public final boolean stmCommitNestedTransaction(TransactionRecord trec) {
        STM.lock(trec);
        TransactionRecord et = trec.enclosingTrec;
        boolean result = validateAndAcquireOwnership(trec, !STM.configUseReadPhase, true);
        if (result) {
            if (STM.configUseReadPhase) {
                result = trec.checkReadOnly();
            }
            if (result) {
                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                loop:
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TransactionEntry e: chunk.entries) {
                        TVar s = e.tvar;
                        if (e.isUpdate()) {
                            s.unlock(trec, e.expectedValue, false);
                        }
                        mergeUpdateInto(et, s, e.expectedValue, e.newValue);
                    }
                }
            } else {
                revertOwnership(trec, false);
            }
        }
        STM.unlock(trec);
        freeTRecHeader(trec);
        return result;
    }

    public final boolean validateAndAcquireOwnership(TransactionRecord trec, boolean acquireAll, boolean retainOwnership) {
        if (STM.shake()) {
            return false;
        } else {
            boolean result = trec.state != TREC_CONDEMNED;
            if (result) {
                ListIterator<StgTRecChunk> cit = trec.chunkIterator();
                loop:
                while (cit.hasPrevious()) {
                    StgTRecChunk chunk = cit.previous();
                    for (TransactionEntry e: chunk.entries) {
                        // Traversal
                        TVar s = e.tvar;
                        if (acquireAll || e.isUpdate()) {
                            if (!s.condLock(trec, e.expectedValue)) {
                                result = false;
                                break loop;
                            }
                        } else {
                            if (RtsFlags.STM.fineGrained) {
                                if (s.currentValue != e.expectedValue) {
                                    result = false;
                                    break loop;
                                }
                                e.numUpdates = s.numUpdates;
                                if (s.currentValue != e. expectedValue) {
                                    result = false;
                                    break loop;
                                }
                            }
                        }
                    }
                }
            }

            if (!result || !retainOwnership) {
                revertOwnership(trec, acquireAll);
            }

            return result;
        }
    }

    public final void revertOwnership(TransactionRecord trec, boolean revertAll) {
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = trec.chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TransactionEntry e: chunk.entries) {
                    if (revertAll || e.isUpdate()) {
                        TVar s = e.tvar;
                        if (s.isLocked(trec)) {
                            s.unlock(trec, e.expectedValue, true);
                        }
                    }

                }
            }
        }
    }
}
