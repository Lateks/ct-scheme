package lib;

public class BuiltIns {
    public static Object display(Object obj) {
        System.out.print(obj);
        return CTUndefined.instance;
    }

    public static Object newline() {
        System.out.println();
        return CTUndefined.instance;
    }

    public static Object toList(Object[] args) {
        Object lst = CTEmptyList.instance;
        for (int i = args.length - 1; i >= 0; --i) {
            lst = cons(args[i], lst);
        }
        return lst;
    }

    public static Object cons(Object a, Object b) {
        return new CTPair(a, b);
    }

    public static boolean toBoolean(Object a) {
        return !(a instanceof CTBool && a == CTBool.falseInstance);
    }
}
