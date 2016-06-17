package ghcvm.runtime.stm;

public enum TRecState {
    TREC_ACTIVE,
    TREC_CONDEMNED,
    TREC_COMMITTED,
    TREC_ABORTED,
    TREC_WAITING
}
