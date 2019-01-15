import test.Hello;
import java.util.Arrays;
import java.util.List;
import java.util.LinkedList;

public class Utils {
    public static void test() {
        Hello h = new Hello();
        List<Integer> l = new LinkedList<Integer>();
        l.add(1); l.add(2); l.add(3);
        System.out.println(Arrays.toString(h.hello(l)));
    }
}
