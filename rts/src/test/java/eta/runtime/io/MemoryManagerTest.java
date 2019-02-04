package eta.runtime.io;

import static eta.runtime.io.MemoryManager.*;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import static java.nio.ByteBuffer.*;

import static java.util.Arrays.*;
import java.util.Random;

import org.junit.Test;
import org.junit.Ignore;
import static org.junit.Assert.*;
import static org.junit.Assume.*;
    
import static org.hamcrest.CoreMatchers.*;

public class MemoryManagerTest {

    Random rnd=new Random();
    
    @Test
    public void copiedByteArraysShouldBeEqual() {
        ByteBuffer b1=buffer(1), b2=buffer(2);
        copyByteBuffer(b1,b2,1);
        assertThat("If you copy all bytes, buffers should be equal",
                   b1,is(b2));
        
        b1=buffer(repeat(10,1));
        b2=buffer(repeat(10,2));
        copyByteBuffer(b1,b2,5);
        ((Buffer)b1).flip();((Buffer)b2).flip();
        assertThat("If you copy some bytes, they should be equal",
                   b2,is(b1));            

        b1=buffer(repeat(10,1));
        b2=buffer(repeat(8,2));
        copyByteBuffer(b1,5,b2,2,5);
        ((Buffer)b2).position(2).limit(7);
        ((Buffer)b1).position(5).limit(10);
        assertThat("If you copy some bytes with offset, they should be equal",
                   b2,is(b1));
    }

    @Test
    public void copyByteArrayToItselfNotChangeIt() {
        ByteBuffer b1=buffer(repeat(10,1));
        ByteBuffer copy=b1.duplicate();
        ByteBuffer res=copyByteBuffer(b1,b1,5);
        assertThat("The returned buffer is the original",
                   b1,is(sameInstance(res)));
        assertTrue("The buffer is equal after copy",
                   ((Buffer)b1).clear().equals(copy));
    }

    @Test
    public void testCopyByteBufferDups() {
        ByteBuffer b1=buffer(repeat(10,1)),
            b2=buffer(repeat(8,2));
        ByteBuffer cpyB1 = b1.duplicate(),
            cpyB2=b2.duplicate();
        ByteBuffer res=copyByteBufferDups(b1,5,b2,2,5);
        assertThat("The returned buffer is NOT the original",
                   b1,is(not(sameInstance(res))));
        assertThat("Position and limit of source does NOT change",
                   b1,is(cpyB1));
        assertThat("Position and limit of dest does NOT change",
                   b2,is(cpyB2));
        ((Buffer)b2).position(2).limit(7);
        ((Buffer)b1).position(5).limit(10);
        assertThat("If you copy some bytes with offset duplicating buffers, "+
                   "they should be equal",
                   b2,is(b1));
    }

    @Test
    public void testAllocateBuffer() {
        int[] sizes = {2,16,256,1024,4096,65536,4194304};
        for (int size : sizes) {
            long address = allocateBuffer(size,true);
            assertTrue("Allocating " + size + " bytes: the address must be greater than 0",
                       address > 0);
        }
    } 

    @Test(expected = IllegalArgumentException.class)
    public void allocateBufferWithNegativeArg() {
        allocateBuffer(-1,true);
    }

    @Test
    public void allocateLargeBuffer() {
        allocateBuffer(4194305,true);
    }
    
    @Test
    public void testGetBoundedBuffer() {
        ByteBuffer b = getBoundedBuffer(0);
        ByteBuffer empty = ByteBuffer.allocate(0);
        assertThat("With zero address it returns an empty buffer",
                   b, is(empty));
        long addr = allocateBuffer(1024, true);
        b = getBoundedBuffer(addr);
        assertTrue("The buffer is as large as the allocated size",
                   b.remaining() >= 1024);
        touch(b,(byte)1);
        ByteBuffer b2 = getBoundedBuffer(addr);
        assertThat("Two invocations with the same adress ,"+
                   "should return the same buffer", b2, is(b));
        b2 = getBoundedBuffer(addr,0,b.remaining());
        assertThat("Calling it with the same address, "+ 
                   "without offset and the same length ,"+
                   "should return the same buffer", b2, is(b));
    }

    @Test(expected = Exception.class)
    @Ignore
    public void testFreeAndGetBuffer() {
        long addr = allocateBuffer(1024, true);
        free(addr);
        getBoundedBuffer(addr);
    }

    
    @Test
    public void testGetBytes() {
        long addr = allocateBuffer(1024, true);
        ByteBuffer b = getBoundedBuffer(addr);
        touch(b,(byte)1);
        byte[] bs = getBytes(addr, 0, b.remaining());
        ByteBuffer b2=ByteBuffer.wrap(bs);
        assertThat("Bytes returned should be equals to the allocated buffer ones",
                   b2,is(b));
    }

    @Test
    public void testSet() {
        long addr = allocateBuffer(16, true);
        ByteBuffer b = getBoundedBuffer(addr,0,16);
        ByteBuffer unset=buffer(new byte[16]);
        assertThat("A new buffer bytes are not set",
                   b,is(unset));
        set(addr,(byte)1,16);
        ByteBuffer set=buffer(repeat(16,1));
        assertThat("The buffer has changed",
                   b,is(set));
        set=buffer(repeat(16,2));
        set(addr,repeat(16,2));
        assertThat("The buffer has changed",
                   b,is(set));
    }

    @Test
    public void testAllocateAndSet() {
        byte[] bs = rndBytes(16);
        long addr = allocateAndSet(bs);
        ByteBuffer b = getBoundedBuffer(addr,0,16);
        ByteBuffer expected = buffer(bs);
        assertThat("The buffer has the expected content",
                   b,is(expected));
    }

    @Test
    public void testMove() {
        byte[] bs= rndBytes(16);
        long addr = allocateAndSet(bs);
        ByteBuffer src = getBoundedBuffer(addr,0,16);
        long addrDest = allocateBuffer(16,true);
        ByteBuffer dest = getBoundedBuffer(addrDest,0,16);
        move(addr,addrDest,16);
        assertThat("The destination buffer has the source one content",
                   dest,is(src));
    }

    @Test
    public void testCompare() {
        byte[] ones = repeat(16,1);
        byte[] twos = repeat(16,2);
        ByteBuffer b1 = buffer(ones);
        ByteBuffer b2 = buffer(twos);
        int cmp = compare(b1.duplicate(),b2.duplicate(),16);
        assertTrue("If the second buffer is greater it should return a negative int",
                   cmp < 0);
        cmp = compare(b2.duplicate(),b1.duplicate(),16);
        assertTrue("If the second buffer is smaller it should return a positive int",
                   cmp > 0);
        b2 = buffer(copyOf(ones,16));
        b2.put(15,(byte)2);
        cmp = compare(b1.duplicate(),b2.duplicate(),16);
        assertTrue("If the last byte of the second buffer is greater, "+
                   "it should return a negative int",
                   cmp < 0);
        cmp = compare(b2.duplicate(),b1.duplicate(),16);
        assertTrue("If the last byte of the second buffer is smaller, "+
                   "it should return a positive int",
                   cmp > 0);
    }

    @Test
    public void testChrIndex() {
        ByteBuffer b = buffer(new byte[16]);
        int result = chrIndex(b.duplicate(), (byte) 1, 16);
        assertThat("If the buffer doesn't contain the value, it returns -1",
                   result, is(-1));
        b.put(15, (byte) 1);
        result = chrIndex(b.duplicate(), (byte) 1, 16);
        assertThat("If the buffer constains the value, it returns its index ",
                   result, is(15));
        b.put(0, (byte) 1);
        result = chrIndex(b.duplicate(), (byte) 1, 16);
        assertThat("If the buffer constains the value, it returns the index " +
                   "of its first occurrence", result, is(0));
        
    }

    @Test
    public void testChr() {
        ByteBuffer b = buffer(new byte[16]);
        ByteBuffer result = chr(b, (byte) 1, 16);
        assertThat("If the buffer doesn't contain the value, "+
                   "it returns an empty buffer",
                   result, is(buffer(new byte[0])));
        assertThat(result, is(not(sameInstance(b))));
        b.put(15,(byte) 1);
        result = chr(b, (byte) 1, 16);
        assertThat("If the buffer contains the value, "+
                   "it returns a buffer pointing out it",
                   result.get(),is((byte)1));
    }

    @Test
    public void testChrOffset() {
        long addr = allocateBuffer(16,true);
        int result = chrOffset(addr,5,16, (byte) 1);
        assertThat("If the buffer doesn't contain the value, "+
                   "it returns the ending offset", result, is(16));
        set(addr+10, (byte) 1, 1);
        result = chrOffset(addr,5,16, (byte) 1);
        assertThat("If the buffer contains the value, "+
                   "it returns its buffer index", result,is(5));
    }

    @Test
    public void testNullAddress() {
        long addr = allocateBuffer(0,true);
        assertThat("If allocating an empty pointer, NULL is returned",
                   addr, is(nullAddress));
        free(0);
    }

    @Test
    public void testNurseryBoundary() {
        allocateBuffer(1024 * 4096,true);
        allocateBuffer(10,true);
    }
    
    // Utils

    private void debug(ByteBuffer b) {
        debug(toString(b));
    }
    
    private String toString(ByteBuffer b) {
        return "{hash:"+b.hashCode()+", pos:"+b.position()+
            ", limit:"+b.limit()+
            ", remaining:"+b.remaining()+
            (b.hasArray()?
             ", bytes:"+byteArrayToHex(b.array()):"")+"}";
    }

    public static String byteArrayToHex(byte[] a) {
        StringBuilder sb = new StringBuilder(a.length * 2);
        for(byte b: a)
            sb.append(String.format("%02x", b));
        return sb.toString();
    }
    
    private void debug(String msg) {
        System.out.println(msg);
    }
    
    private ByteBuffer buffer(byte... bs) {
        return wrap(bs);
    }

    private ByteBuffer buffer(int... is) {
        byte[] bs= new byte[is.length];
        for (int i=0;i<is.length;i++)
            bs[i]=(byte)is[i];
        return wrap(bs);
    }

    private byte[] rndBytes(int length) {
        byte[] bs=new byte[length];
        rnd.nextBytes(bs);
        return bs;
    }

    private byte[] repeat(int length,int val) {
        byte[] bs = new byte[length];
        fill(bs,(byte)val);
        return bs;
    }

    private void touch(ByteBuffer b,byte val) {
        b.duplicate().put(val); 
    }
}
