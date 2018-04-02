package eta.runtime.thunk;

import eta.Thunk;
import java.lang.reflect.Field;
import java.util.WeakHashMap;

public class Thunks {

    /* TODO: Make this local caching and have a better heuristic for selecting
             to cache a lookup. Moreover, make the cache thread safe. */

    /* Used to facilitate the free variable clearing code and caches the field
       lookups to reduce the cost of reflection. */
    private static WeakHashMap<Class<?>, Field[]> thunkFieldsCache
        = new WeakHashMap<Class<?>, Field[]>();

    public static Field[] lookupFields(Class<? extends Thunk> clazz) {
        Field[] fields = thunkFieldsCache.get(clazz);
        int i = 0;
        if (fields == null) {
            Field[] lookupFields = clazz.getFields();
            for (Field f:lookupFields) {
                if (canClearField(f)) {
                    i++;
                }
            }
            fields = new Field[i];
            i = 0;
            for (Field f:lookupFields) {
                if (canClearField(f)) {
                    fields[i++] = f;
                }
            }
            thunkFieldsCache.put(clazz, fields);
        }
        return fields;
    }

    private static boolean canClearField(Field f) {
        return !f.getName().equals("indirectee")
            && !f.getType().isPrimitive();
    }


}
