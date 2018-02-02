package eta.runtime.io;

import static eta.runtime.io.MemoryManager.*;

import java.nio.ByteBuffer;
import static java.nio.ByteBuffer.*;

import static java.util.Arrays.*;
import java.util.Arrays;
import java.util.Random;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.junit.Assume.*;
    
import static org.hamcrest.CoreMatchers.*;

public class MemoryManagerTest {

    Random rnd=new Random();
    
    @Test
    public void copiedByteArraysShouldBeEqual() {
        ByteBuffer b1=buffer(1), b2=buffer(2);
        copyByteBuffer(b1,b2,1);
        assertTrue("If you copy all bytes, buffers should be equal",
                   b1.equals(b2));
        
        b1=buffer(repeat(10,1));
        b2=buffer(repeat(10,2));
        copyByteBuffer(b1,b2,5);
        b1.flip();b2.flip();
        assertTrue("If you copy some bytes, they should be equal",
                   b1.equals(b2));            

        b1=buffer(repeat(10,1));
        b2=buffer(repeat(8,2));
        copyByteBuffer(b1,5,b2,2,5);
        b2.position(2).limit(7);
        b1.position(5).limit(10);
        assertTrue("If you copy some bytes with offset, they should be equal",
                   b1.equals(b2));
    }

    @Test
    public void copyByteArrayToItselfNotChangeIt() {
        ByteBuffer b1=buffer(repeat(10,1));
        ByteBuffer copy=b1.duplicate();
        ByteBuffer res=copyByteBuffer(b1,b1,5);
        assertThat("The returned buffer is the original",
                   b1,is(sameInstance(res)));
        assertTrue("The buffer is equal after copy",
                   b1.clear().equals(copy));
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
        assertTrue("But the content change",
                   Arrays.equals(b1.array(),cpyB1.array()) &&
                   Arrays.equals(b2.array(),cpyB2.array()));
    }

    // Utils
    private String toString(ByteBuffer b) {
        return "{pos:"+b.position()+", limit:"+b.limit()+
            ", remaining:"+b.remaining()+
            ", bytes:"+byteArrayToHex(b.array())+"}";
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
              

    
}
