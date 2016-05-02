package lib;

public class BuiltIns {
    private static String getTypeName(Object n) {
        if (n instanceof CTObject) {
            CTObject param = (CTObject)n;
            return param.getTypeName();
        } else {
            return "procedure";
        }
    }

    private static void assertNumber(String procedureName, Object n) {
        if (!(n instanceof CTNumber)) {
            String typeName = getTypeName(n);
            throw new TypeError(procedureName, CTNumber.typeName, typeName);
        }
    }

    private static void assertPair(String procedureName, Object n) {
        if (!(n instanceof CTPair)) {
            String typeName = getTypeName(n);
            throw new TypeError(procedureName, CTPair.typeName, typeName);
        }
    }

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

    public static Object car(Object a) {
        assertPair("car", a);
        CTPair p = (CTPair) a;
        return p.getCar();
    }

    public static Object cdr(Object a) {
        assertPair("cdr", a);
        CTPair p = (CTPair) a;
        return p.getCdr();
    }

    public static boolean toBoolean(Object a) {
        return !(a instanceof CTBool && a == CTBool.falseInstance);
    }

    public static Object isZero(Object a) {
        assertNumber("zero?", a);
        CTNumber n = (CTNumber)a;
        return CTBool.toCTBool(n.getValue() == 0);
    }

    public static Object isNull(Object a) {
        return CTBool.toCTBool(a instanceof CTEmptyList);
    }

    public static Object not(Object a) {
        return CTBool.toCTBool(!toBoolean(a));
    }

    public static Object areEq(Object a, Object b) {
        return CTBool.toCTBool(a.equals(b));
    }
}
