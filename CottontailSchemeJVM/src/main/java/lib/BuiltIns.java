package lib;

public class BuiltIns {
    private static void assertNumber(String procedureName, Object n) {
        if (!(n instanceof CTNumber)) {
            String typeName = "procedure";
            if (n instanceof CTObject) {
                CTObject param = (CTObject)n;
                typeName = param.getTypeName();
            }
            throw new TypeError(procedureName, CTNumber.typeName, typeName);
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
}
