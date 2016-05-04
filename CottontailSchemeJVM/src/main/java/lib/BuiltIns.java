package lib;

import java.lang.invoke.MethodHandle;
import java.util.Comparator;

public class BuiltIns {
    public static final String procNameCar = "car";
    public static final String procNameCdr = "cdr";
    public static final String procNameCons = "cons";
    public static final String procNameDisplay = "display";
    public static final String procNameEq = "eq?";
    public static final String procNameList = "list";
    public static final String procNameNewline = "newline";
    public static final String procNameNot = "not";
    public static final String procNameNull = "null?";
    public static final String procNameZero = "zero?";
    public static final String procNamePlus = "+";
    public static final String procNameMinus = "-";
    public static final String procNameMult = "*";
    public static final String procNameDiv = "/";
    public static final String procNameLessThan = "<";
    public static final String procNameGreaterThan = ">";

    private static String getTypeName(Object n) {
        if (n instanceof CTObject) {
            CTObject param = (CTObject)n;
            return param.getTypeName();
        } else {
            return "procedure";
        }
    }

    private static void assertParameterCountAtLeast(String procedureName, int n, Object[] args) {
        if (args.length < n)
            throw new ArityMismatchError(procedureName, "at least " + n, args.length);
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
        if (obj instanceof MethodHandle) {
            System.out.print("<#procedure>");
        } else {
            System.out.print(obj);
        }
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
        assertPair(procNameCar, a);
        CTPair p = (CTPair) a;
        return p.getCar();
    }

    public static Object cdr(Object a) {
        assertPair(procNameCdr, a);
        CTPair p = (CTPair) a;
        return p.getCdr();
    }

    public static boolean toBoolean(Object a) {
        return !(a instanceof CTBool && a == CTBool.falseInstance);
    }

    public static Object isZero(Object a) {
        assertNumber(procNameZero, a);
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

    public static Object plus(Object[] args) {
        double sum = 0;
        for (Object arg : args) {
            assertNumber(procNamePlus, arg);
            sum += getNumberValue(arg);
        }
        return new CTNumber(sum);
    }

    public static Object minus(Object[] args) {
        for (Object arg : args) {
            assertNumber(procNameMinus, arg);
        }
        assertParameterCountAtLeast(procNameMinus, 1, args);

        double result = 0;
        if (args.length == 1) {
            result = -getNumberValue(args[0]);
        } else {
            result = getNumberValue(args[0]);
            for (int i = 1; i < args.length; ++i) {
                result -= getNumberValue(args[i]);
            }
        }
        return new CTNumber(result);
    }

    public static Object mult(Object[] args) {
        double result = 1;

        for (Object arg : args) {
            assertNumber(procNameMult, arg);
            result *= getNumberValue(arg);
        }

        return new CTNumber(result);
    }

    public static Object div(Object[] args) {
        for (Object arg : args) {
            assertNumber(procNameDiv, arg);
        }
        assertParameterCountAtLeast(procNameDiv, 1, args);

        double result = 0;
        if (args.length == 1) {
            result = 1.0 / getNumberValue(args[0]);
        } else {
            result = getNumberValue(args[0]);
            for (int i = 1; i < args.length; ++i) {
                result /= getNumberValue(args[i]);
            }
        }

        return new CTNumber(result);
    }

    public static Object lessThan(Object[] args) {
        for (Object arg : args) {
            assertNumber(procNameLessThan, arg);
        }
        assertParameterCountAtLeast(procNameLessThan, 1, args);

        return comparePairs(args, Comparator.naturalOrder());
    }

    public static Object greaterThan(Object[] args) {
        for (Object arg : args) {
            assertNumber(procNameGreaterThan, arg);
        }
        assertParameterCountAtLeast(procNameGreaterThan, 1, args);

        return comparePairs(args, Comparator.reverseOrder());
    }

    private static Object comparePairs(Object[] args, Comparator<Double> comp) {
        double prev = getNumberValue(args[0]);
        for (int i = 1; i < args.length; ++i) {
            double val = getNumberValue(args[i]);
            if (comp.compare(prev, val) < 0) {
                prev = val;
            } else {
                return CTBool.falseInstance;
            }
        }
        return CTBool.trueInstance;
    }

    private static double getNumberValue(Object o) {
        return ((CTNumber) o).getValue();
    }

}
