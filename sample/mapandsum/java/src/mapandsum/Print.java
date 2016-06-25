package mapandsum;

import ghcvm.runtime.stg.StgContext;

public class Print {
    public static void printIntzh(StgContext context) {
        int i = context.I(1);
        System.out.println(i);
    }
}
