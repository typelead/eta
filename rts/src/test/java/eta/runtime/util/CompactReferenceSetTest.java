package eta.runtime.util;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class CompactReferenceSetTest {

    CompactReferenceSet<Object> set;

    @Before
    public void init() {
        set = new CompactReferenceSet<Object>(128);
    }
    
    public static class HashObject {
        private int hash;

        public HashObject(int hash) {
            this.hash = hash;
        }
        
        @Override
        public int hashCode() {
            return hash;
        }
    }

    @Test
    public void testInsertionDeletion() {
        final Object object1 = new Object();
        final Object object2 = new Object();
        assertEquals(0, set.size());
        assertEquals(true, set.add(object1));
        assertEquals(1, set.size());
        assertEquals(true, set.contains(object1));
        assertEquals(false, set.add(object1));
        assertEquals(1, set.size());
        assertEquals(true, set.remove(object1));
        assertEquals(0, set.size());
        assertEquals(false, set.contains(object1));
        assertEquals(false, set.remove(object1));
        assertEquals(false, set.remove(object2));
        assertEquals(0, set.size());
        set.add(object1);
        assertEquals(1, set.size());
        set.add(object2);
        assertEquals(2, set.size());
        assertEquals(true, set.contains(object1) && set.contains(object2));
        set.clear();
        assertEquals(0, set.size());
        assertEquals(false, set.contains(object1) || set.contains(object2));
        assertEquals(false, set.remove(object1)   || set.remove(object2));
        assertEquals(0, set.size());
    }
    
    @Test
    public void testHashCollisions() {
        final int num = 25;
        final HashObject[] objects = new HashObject[num];
        for (int i = 0; i < num; i++) {
            assertEquals(i, set.size());
            assertEquals(true, set.add(objects[i] = new HashObject(16 << i)));
        }

        assertEquals(num, set.size());

        for (int i = 0; i < num; i++) {
            assertEquals(true, set.contains(objects[i]));
        }

        for (int i = 0; i < num; i++) {
            assertEquals(num, set.size());
            assertEquals(false, set.add(objects[i]));
        }


        for (int i = 0; i < num; i++) {
            assertEquals(num - i, set.size());
            assertEquals(true, set.remove(objects[i]));
        }

        for (int i = 0; i < num; i++) {
            assertEquals(0, set.size());
            assertEquals(false, set.remove(objects[i]));
        }
    }
}
