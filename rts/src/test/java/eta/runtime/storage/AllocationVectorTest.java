package eta.runtime.storage;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static eta.runtime.TestUtils.*;

public class AllocationVectorTest {

    AllocationVector av;

    @Before
    public void init() {
        av = new AllocationVector(16);
    }

    @Test
    public void testAllocVecInit12() {
        assertArrayEquals(new AllocationVector(12).getBytes()
                         ,new byte[] { 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecInit16() {
        assertArrayEquals(new AllocationVector(16).getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecAllocateSingle() {
        av.allocate(0, 4);
        assertArrayEquals(av.getBytes()
                         ,new byte[] { (byte) 0xBF, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecAllocateMultiple() {
        av.allocate(0, 2);
        av.allocate(2, 3);
        assertArrayEquals(av.getBytes()
                         ,new byte[] { (byte) 0xFB, 0x2, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecFreeStart() {
        av.allocate(0, 4);
        assertEquals(4, av.free(0));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecFreeEnd() {
        av.allocate(12, 4);
        assertEquals(4, av.free(12));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecFreeMiddle() {
        av.allocate(4, 2);
        av.allocate(6, 2);
        av.allocate(8, 4);
        assertEquals(2, av.free(6));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x4b, (byte) 0xbf, 0x40 });
    }

    @Test
    public void testAllocVecFreeCoalesce() {
        av.allocate(4, 2);
        av.allocate(6, 2);
        av.allocate(8, 4);
        av.allocate(12, 2);
        assertEquals(2, av.free(6));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x4b, (byte) 0xbf, 0x4b });
        assertEquals(4, av.free(8));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0b, (byte) 0x40, 0x4b });
        assertEquals(2, av.free(4));
        assertEquals(2, av.free(12));
        assertArrayEquals(av.getBytes()
                         ,new byte[] { 0x0, 0x0, 0x0, 0x40 });
    }

    @Test
    public void testAllocVecFreeAndAllocateBoundaries() {
        assertEquals(-1, av.findFreeAndAllocate(20));
        assertEquals(-1, av.findFreeAndAllocate(17));
        assertEquals(0, av.findFreeAndAllocate(16));
        assertEquals(-1, av.findFreeAndAllocate(1));
    }

    @Test
    public void testAllocVecFreeAndAllocateDynamic() {
        assertEquals(0,  av.findFreeAndAllocate(1));
        assertEquals(1,  av.findFreeAndAllocate(2));
        assertEquals(3,  av.findFreeAndAllocate(3));
        assertEquals(6,  av.findFreeAndAllocate(7));
        assertEquals(13, av.findFreeAndAllocate(2));
        assertEquals(3,  av.free(3));
        assertEquals(3,  av.findFreeAndAllocate(3));
        assertEquals(15,  av.findFreeAndAllocate(1));
        assertEquals(1,  av.free(0));
        assertEquals(0,  av.findFreeAndAllocate(1));
    }
}
