package eta.runtime;

import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static eta.runtime.TestUtils.*;

import eta.runtime.storage.AllocationVector;

public class AllocationVectorTest {

    @Test
    public void testAllocVecInit12() {
        assertArrayEquals(new AllocationVector(12).getBytes()
                          ,new byte[] { 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecInit13() {
        assertArrayEquals(new AllocationVector(13).getBytes()
                          ,new byte[] { 0x0, 0x0, 0x0, 0x1 });
    }


    @Test
    public void testAllocVecInit14() {
        assertArrayEquals(new AllocationVector(14).getBytes()
                          ,new byte[] { 0x0, 0x0, 0x0, 0x4 });
    }

    @Test
    public void testAllocVecInit15() {
        assertArrayEquals(new AllocationVector(15).getBytes()
                          ,new byte[] { 0x0, 0x0, 0x0, 0x10 });
    }

    @Test
    public void testAllocVecInit16() {
        assertArrayEquals(new AllocationVector(16).getBytes()
                          ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecAllocateSingle() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(0, 4);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { (byte) 0xBF, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecAllocateMultiple() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(0, 2);
        av.allocate(2, 3);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { (byte) 0xFB, 0x2, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecFreeStart() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(0, 4);
        int blocks = av.free(0);
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
        assertEquals(4, blocks);
    }

    @Test
    public void testAllocVecFreeEnd() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(12, 4);
        int blocks = av.free(12);
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
        assertEquals(4, blocks);
    }

    @Test
    public void testAllocVecFreeMiddle() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(4, 2);
        av.allocate(6, 2);
        av.allocate(8, 4);
        int blocks = av.free(6);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { 0x0, 0x4b, (byte) 0xbf, 0x40 });
        assertEquals(2, blocks);
    }

    @Test
    public void testAllocVecFreeCoalesce() {
        AllocationVector av = new AllocationVector(16);
        av.allocate(4, 2);
        av.allocate(6, 2);
        av.allocate(8, 4);
        av.allocate(12, 2);
        int blocks = av.free(6);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { 0x0, 0x4b, (byte) 0xbf, 0x4b });
        assertEquals(2, blocks);
        blocks = av.free(8);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { 0x0, 0x0b, (byte) 0x40, 0x4b });
        assertEquals(4, blocks);
        av.free(4);
        av.free(12);
        assertArrayEquals(av.getBytes()
                          ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }


}
