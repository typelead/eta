package ghcvm.runtime.stm;

import java.util.List;
import java.util.ArrayList;

public class StgTRecChunk {
    public static final int TREC_CHUNK_NUM_ENTRIES = 16;
    public int nextEntryIndex;
    public List<TRecEntry> entries = new ArrayList<TRecEntry>(16);
}
