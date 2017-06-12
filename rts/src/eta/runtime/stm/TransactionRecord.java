package eta.runtime.stm;

import java.util.Stack;
import java.util.Queue;
import java.util.ArrayDeque;
import java.util.ListIterator;

import eta.runtime.RtsFlags;
import eta.runtime.stg.Closure;

public class TransactionRecord implements Iterable<TransactionEntry> {
    public TransactionRecord enclosingTrec;
    public Map<TVar, TransactionEntry> entries = new HashMap<TVar, TransactionEntry>();
    public Map<AtomicInvariant, InvariantCheck> invariantsToCheck
        = new LinkedHashMap<AtomicInvariant, InvariantCheck>();
    public TransactionRecordState state;
    public enum TransactionRecordState {
        TREC_ACTIVE,
        TREC_CONDEMNED,
        TREC_COMMITTED,
        TREC_ABORTED,
        TREC_WAITING
    }

    public TransactionRecord(TransactionRecord enclosingTrec) {
        this.enclosingTrec = enclosingTrec;
        if (enclosingTrec == null) {
            this.state = TREC_ACTIVE;
        } else {
            assert enclosingTrec.state == TREC_ACTIVE
                || enclosingTrec.state == TREC_CONDEMNED;
            this.state = enclosingTrec.state;
        }
    }

    public static TransactionRecord start(TransactionRecord enclosing) {
        return new TransactionRecord(enclosing);
    }

    public static class EntrySearchResult {
        public final TransactionRecord header;
        public final TransactionEntry entry;
        public EntrySearchResult(final TransactionRecord header, final TransactionEntry entry) {
            this.header = header;
            this.entry = entry;
        }
    }

    public TransactionEntry getNested(TVar tvar) {
        TransactionEntry entry = null;
        TransactionRecord trec = this;
        do {
            entry = get(tvar);
        } while (entry == null && ((trec = trec.enclosingTrec) != null));
        return (entry == null? null:new EntrySearchResult(trec, entry));
    }

    public TransactionEntry get(TVar tvar) {
        return entries.get(tvar);
    }

    public void put(TVar tvar, Closure expected, Closure updated) {
        entries.put(new TransactionEntry(tvar, expected, updated));
    }

    @Override
    public Iterator<TransactionEntry> iterator() {
        return entries.values().iterator();
    }

    public void checkInvariant(Closure invariantCode) {
        invariantsToCheck
            .offerFirst(new InvariantCheck(new AtomicInvariant(invariantCode)));
    }

    public boolean checkReadOnly() {
        boolean result = true;
        if (RtsFlags.STM.fineGrained) {
            ListIterator<StgTRecChunk> cit = chunkIterator();
            loop:
            while (cit.hasPrevious()) {
                StgTRecChunk chunk = cit.previous();
                for (TransactionEntry e: chunk.entries) {
                    TVar s = e.tvar;
                    if (e.isReadOnly()) {
                        if (s.currentValue != e.expectedValue ||
                            s.numUpdates != e.numUpdates) {
                            result = false;
                            break loop;
                        }
                    }
                }
            }
        }
        return result;
    }

    public final void connectInvariant(AtomicInvariant inv) {
        /* ASSERT (inv.lastExection == null) */
        ListIterator<StgTRecChunk> cit = chunkIterator();
        while (cit.hasPrevious()) {
            StgTRecChunk chunk = cit.previous();
            for (TransactionEntry e: chunk.entries) {
                TVar s = e.tvar;
                EntrySearchResult result = STM.getEntry(enclosingTrec, s);
                TransactionEntry entry = result.entry;
                if (entry != null) {
                    e.expectedValue = entry.newValue;
                    e.newValue = entry.newValue;
                }
                /* TODO: Verify order */
                s.watchQueue.offer(inv);
            }
        }
        inv.lastExecution = this;
    }

    public Queue<InvariantCheck> getInvariantsToCheck(Collection<InvariantCheck> drainTo) {
        assert state == TREC_ACTIVE
            || state == TREC_WAITING
            || state == TREC_CONDEMNED;
        assert enclosingTrec == null;
        /* This loop checks for any invariants that are connected to the TVar
           and adds them to invariants to check for the TRec if they don't
           exist already.
        */
        for(TransactionEntry e: entries.values()) {
            if (e.isUpdate()) {
                TVar s = e.tvar;
                Closure old = s.lock(this);
                for (AtomicInvariant inv: s.getInvariants()) {
                    if (invariantsToCheck.get(inv) == null) {
                        invariantsToCheck.put(inv, new InvariantCheck(inv));
                    }
                }
                s.unlock(this, old);
            }
        }
        return drainTo.addAll(invariantsToCheck.values());
    }

    public void abort() {
        assert state == TREC_ACTIVE
            || state == TREC_WAITING
            || state == TREC_CONDEMNED;
        if (enclosingTrec == null) {
            if (state == TREC_WAITING) {
                removeWatchQueueEntries();
            }
        } else {
            for (TransactionEntry e: entries.values()) {
                enclosingTrec.mergeReadInto(e.tvar, e.expectedValue);
            }
        }
    }

    public void mergeReadInto(TVar tvar, Closure expectedValue) {
        TransactionRecord t = this;
        TransactionEntry e = null;
        do {
            e = t.get(tvar);
            if (e != null && e.expectedValue != expectedValue) {
                t.state = TREC_CONDEMNED;
            }
        } while (e == null && ((t = t.enclosingTrec) != null));
        if (e == null) {
            put(tvar, expectedValue, expectedValue);
        }
    }

    public void removeWatchQueueEntries() {
        assert enclosingTrec == null;
        assert state == TREC_WAITING
            || state == TREC_CONDEMNED;
        for (TransactionEntry e:entries) {
            TVar s = e.tvar;
            Closure saw = s.lock(this);
            assert s.currentValue == this;
            s.removeFromWatchQueue((TSO) e.newValue);
            s.unlock(this, saw);
        }
    }

    public boolean commit(Capability cap) {
        assert enclosingTrec == null;
        assert state == TREC_ACTIVE
            || state == TREC_CONDEMNED;
        boolean touchedInvariants = !invariantsToCheck.isEmpty();
        if (touchedInvariants) {
            for (InvariantCheck q: invariantsToCheck.values()) {
                AtomicInvariant inv = q.invariant;
                if (!inv.lock()) {
                    state = TREC_CONDEMNED;
                    break;
                }
                TransactionRecord oldTrec = inv.lastExecution;
                if (oldTrec != null) {
                    for (TransactionEntry e:oldTrec) {
                        mergeReadInto(e.tvar, e.expectedValue);
                    }
                }
            }
        }
        boolean useReadPhase = !touchedInvariants;
        boolean result = validateAndAcquireOwnership(!useReadPhase, true);
        if (result) {
            assert state == TREC_ACTIVE;
            if (useReadPhase) {
                /* TODO: Handle token verification here. We currently bypass the
                         need for this by using long's, so we are essentially
                         guaranteed to avoid overflows. */
            }
        }
        if (result) {
            if (touchedInvariants) {
                for (InvariantCheck q: invariantsToCheck.values()) {
                    AtomicInvariant inv = q.invariant;
                    if (inv.lastExecution != null) {
                        inv.disconnect();
                    }
                    connectInvariant(inv, q.myExecution);
                    inv.unlock();
                }
            }

            for (TransactionEntry e: entries.values()) {
                TVar s = e.tvar;
                if (!useReadPhase || e.isUpdate()) {
                    assert s.isLocked(this);
                    s.unparkWaiters(cap);
                    s.numUpdates++;
                    s.unlock(e.newValue);
                }
                assert !s.isLocked(this);
            }
        } else {
            revertOwnership(false);
        }
        return result;
    }

    public boolean validateAndAcquireOwnership(boolean acquireAll, boolean retainOwnership) {
        assert state == TREC_ACTIVE
            || state == TREC_WAITING
            || state == TREC_CONDEMNED;
        boolean result = !(state == TREC_CONDEMNED);
        if (result) {
            for (TransactionEntry e:entries.values()) {
                TVar s = e.tvar;
                if (acquireAll || e.isUpdate()) {
                    if (!s.conditionalLock(trec, e.expectedValue)) {
                        result = false;
                        break;
                    }
                } else {
                    if (s.currentValue != e.expectedValue) {
                        result = false;
                        break;
                    }
                    e.numUpdates = s.numUpdates;
                    if (s.currentValue != e.expectedValue) {
                        result = false;
                        break;
                    }
                }
            }
        }

        if (!result || !retainOwnership) {
            trec.revertOwnership(acquireAll);
        }
        return result;
    }

    public void revertOwnership(boolean revertAll) {
        for (TransactionEntry e:entries.values()) {
            if (revertAll || e.isUpdate()) {
                TVar s = e.tvar;
                if (s.isLocked(this)) {
                    tvar.unlock(e.expectedValue);
                }
            }
        }
    }

    public void connectInvariant(AtomicInvariant inv, TransactionRecord myExecution) {
        assert inv.lastExecution == null;
        for (TransactionEntry e:myExecution) {
            TVar s = e.tvar;
            EntrySearchResult result = myExecution.enclosingTrec.getNested(s);
            if (result != null) {
                TransactionEntry entry = result.entry;
                e.expectedValue = entry.newValue;
                e.newValue = entry.newValue;
            }
            s.addInvariant(inv);
        }
        inv.lastExecution = myExecution;
    }

    public boolean validateNestOfTransactions() {
        assert state == TREC_ACTIVE
            || state == TREC_WAITING
            || state == TREC_CONDEMNED;
        TransactionRecord t = this;
        boolean result = true;
        while (t != null) {
            /* TODO: Can this be optimized to break at result = false? */
            result &= t.validateAndAcquireOwnership(true, false);
            t = t.enclosingTrec;
        }

        if (!result && state != TREC_WAITING) {
            state = TREC_CONDEMNED;
        }
        return result;
    }

    public boolean wait(TSO tso) {
        boolean valid = validateAndAcquireOwnership(true, true);
        if (valid) {
            trec.buildWatchQueueEntries(tso);
            tso.park();
            trec.state = TREC_WAITING;
        }
        return valid;
    }

    public boolean reWait(TSO tso) {
        assert enclosingTrec == null;
        assert state == TREC_WAITING
            || state == TREC_CONDEMNED;
        boolean valid = validateAndAcquireOwnership(true, true);
        if (valid) {
            assert state == TREC_WAITING;
            tso.park();
            revertOwnership(true);
        } else {
            if (state != TREC_CONDEMNED) {
                trec.removeWatchQueueEntries();
            }
        }
        return valid;
    }

    public void buildWatchQueueEntries(TSO tso) {
        assert enclosingTrec == null;
        assert state == TREC_ACTIVE;
        for (TransactionEntry e:entries.values()) {
            TVar s = e.tvar;
            assert s.currentValue == this;
            s.offerWatchQueue(tso);
            e.newValue = tso;
        }
    }

    public void condemn() {
        assert state == TREC_ACTIVE
            || state == TREC_WAITING
            || state == TREC_CONDEMNED;
        if (state == TREC_WAITING) {
            assert enclosingTrec == null;
            trec.removeWatchQueueEntries();
        }
        state = TREC_CONDEMNED;
    }

    public void commitNested() {
        assert enclosingTrec != null;
        assert state == TREC_ACTIVE || state == TREC_CONDEMNED;
        boolean valid = validateAndAcquireOwnership(false, true);
        if (valid) {
            valid = checkReadOnly();
            if (valid) {
                for (TransactionEntry e:entries.values()) {
                    TVar s = e.tvar;
                    if (e.isUpdate()) {
                        s.unlock(e.expectedValue);
                    }
                    enclosingTrec.mergeUpdateInto(s, e.expectedValue, e.newVale);
                    assert s.currentValue != this;
                }
            } else {
                revertOwnership(false);
            }
        }
        return valid;
    }

    public void mergeUpdateInto(TVar tvar, Closure expectedValue, Closure newValue) {
        TransactionEntry e = get(tvar);
        if (e == null) {
            put(tvar, expectedValue, newValue);
        } else {
            if (e.expectedValue != expectedValue) {
                state = TREC_CONDEMNED;
            }
            e.newValue = newValue;
        }
    }

    public boolean checkReadOnly() {
        boolean valid = true;
        for (TransactionEntry e:entries.values()) {
            TVar s = e.tvar;
            if (e.isReadOnly()) {
                if (s.currentValue != e.expectedValue ||
                    s.numUpdates != e.numUpdates) {
                    valid = false;
                    break;
                }
            }
        }
        return valid;
    }
}
