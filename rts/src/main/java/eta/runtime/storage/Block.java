package eta.runtime.storage;

import java.util.List;
import java.nio.Buffer;
import java.nio.ByteBuffer;

import eta.runtime.stg.Capability;

public class Block {
    private long startAddress;
    private ByteBuffer buffer;
    private AllocationVector av;
    private Capability owner;

    /* This link is used to store it in the collection of free blocks,
       and to store the link to the previous buffer. */
    public Block link;

    public Block() {}

    public final long allocate(int miniblocks) {
        int index = av.allocate(miniblocks);
        if (index == -1) return 0;
        else return startAddress + getMiniBlockSize() * index;
    }

    public final boolean isActive() {
        return buffer != null;
    }

    private final int getMiniBlockSize() {
        return buffer.remaining() / av.size();
    }

    public final long getAddress() {
        return startAddress;
    }

    public final int getSize() {
        return buffer.remaining();
    }

    public final void setLink(Block link) {
        this.link = link;
    }

    public final Block getLink() {
        return link;
    }

    public final void init(long startAddress, int miniBlockSize, ByteBuffer buffer) {
        this.startAddress = startAddress;
        this.av           = new AllocationVector(buffer.remaining() / miniBlockSize);
        this.buffer       = buffer;
        this.owner        = Capability.getLocal();
    }

    public final void initWith(Block source) {
        if (this != source) {
            this.startAddress = source.startAddress;
            this.av           = source.av;
            this.buffer       = source.buffer;
            this.owner        = source.owner;
        }
    }

    private final long findFreeBlocks(int miniblocks, boolean direct) {
        if (buffer.isDirect() != direct) return 0;
        int index = av.findFreeAndAllocate(miniblocks);
        if (index == -1) return 0;
        return startAddress + index * getMiniBlockSize();
    }

    public static long findFreeInBlockStack(Block block, int miniblocks, boolean direct) {
        while (block != null) {
            long address = block.findFreeBlocks(miniblocks, direct);
            if (address != 0) return address;
            block = block.link;
        }
        return 0;
    }

    public final void sendFreeMessage(long address) {
        if (owner == Capability.getLocal()) {
            free(address);
        } else {
            owner.free(address);
        }
    }

    public final void free(long address) {
        av.free((int)(address - startAddress)
            >>> Integer.numberOfTrailingZeros(getMiniBlockSize()));
    }

    public final ByteBuffer getBoundedBuffer(long address) {
        return (ByteBuffer) ((Buffer) buffer.duplicate()).position((int)(address - startAddress));
    }

    public final int allocatedSize(long address) {
        int miniBlockSize = getMiniBlockSize();
        int idx = (int)(address - startAddress) / miniBlockSize;
        return av.allocatedSize(idx) * miniBlockSize;
    }

    /* Reading from/writing to memory. */
    public final byte get(long address) {
        return buffer.get((int)(address - startAddress));
    }

    public final short getShort(long address) {
        return buffer.getShort((int)(address - startAddress));
    }

    public final char getChar(long address) {
        return buffer.getChar((int)(address - startAddress));
    }

    public final int getInt(long address) {
        return buffer.getInt((int)(address - startAddress));
    }

    public final long getLong(long address) {
        return buffer.getLong((int)(address - startAddress));
    }

    public final float getFloat(long address) {
        return buffer.getFloat((int)(address - startAddress));
    }

    public final double getDouble(long address) {
        return buffer.getDouble((int)(address - startAddress));
    }

    public final void put(long address, byte val) {
        buffer.put((int)(address - startAddress), val);
    }

    public final void putShort(long address, short val) {
        buffer.putShort((int)(address - startAddress), val);
    }

    public final void putChar(long address, char val) {
        buffer.putChar((int)(address - startAddress), val);
    }

    public final void putInt(long address, int val) {
        buffer.putInt((int)(address - startAddress), val);
    }

    public final void putLong(long address, long val) {
        buffer.putLong((int)(address - startAddress), val);
    }

    public final void putFloat(long address, float val) {
        buffer.putFloat((int)(address - startAddress), val);
    }

    public final void putDouble(long address, double val) {
        buffer.putDouble((int)(address - startAddress), val);
    }

    /* Caching */
    public final void fillCache(long address, CachedBlock cb) {
        if (buffer == null) {
            throw new IllegalStateException
              ("Attempted to access memory address " + address +
               " which has not be allocated yet!");
        }
        cb.set(this, startAddress, startAddress + buffer.remaining());
    }

    /* Monitoring */
    public final BlockStats getStatistics() {
        int miniBlockSize = getMiniBlockSize();
        ByteBuffer buffer = this.buffer.duplicate();
        List<Span> spans  = av.getSpans();
        for (Span span: spans) {
            int numBytes = span.size * miniBlockSize;
            if (span.allocated) {
                byte[] bytes = new byte[numBytes];
                buffer.get(bytes);
                span.bytes = bytes;
            } else {
                ((Buffer)buffer).position(buffer.position() + numBytes);
            }
        }
        return new BlockStats(startAddress, owner.getId(), buffer.isDirect(), spans);
    }
}
