package eta.runtime.storage;

import eta.runtime.stg.Capability;
import java.nio.ByteBuffer;

public class Block {
    private long startAddress;
    private long nextAddress;
    private int size;
    private ByteBuffer buffer;
    private int bufferOffset;

    /* This link is used to store it in the collection of free blocks,
       and to store the link to the previous buffer. */
    public Block link;

    public Block() {}

    public Block(long startAddress, int size, ByteBuffer buffer, int bufferOffset) {
        init(startAddress, size, buffer, bufferOffset);
    }

    public long allocate(int n, Capability cap) {
        if (nextAddress + n > size) {
            insertFreeBlock(cap);
            return 0;
        }
        long address = nextAddress;
        nextAddress += n;
        return address;
    }

    public void insertFreeBlock(Capability cap) {
        int offset = (int)(nextAddress - startAddress);
        int remaining = size - offset;
        cap.insertFreeBlock(new Block(nextAddress, remaining, buffer, bufferOffset + offset), buffer.isDirect());
    }

    public long getAddress() {
        return startAddress;
    }

    public void setLink(Block link) {
        this.link = link;
    }

    public Block getLink() {
        return link;
    }

    public int remaining() {
        return size - (int)(nextAddress - startAddress);
    }

    public void init(long startAddress, int size, ByteBuffer buffer, int bufferOffset) {
        this.startAddress = startAddress;
        this.nextAddress  = startAddress;
        this.size = size;
        this.buffer = buffer;
        this.bufferOffset = bufferOffset;
    }
}
