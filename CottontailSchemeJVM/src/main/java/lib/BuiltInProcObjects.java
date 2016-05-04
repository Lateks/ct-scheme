package lib;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class BuiltInProcObjects {
    public static final MethodHandle car;
    public static final MethodHandle cdr;
    public static final MethodHandle cons;
    public static final MethodHandle display;
    public static final MethodHandle areEq;
    public static final MethodHandle toList;
    public static final MethodHandle newline;
    public static final MethodHandle not;
    public static final MethodHandle isNull;
    public static final MethodHandle isZero;
    public static final MethodHandle plus;
    public static final MethodHandle minus;
    public static final MethodHandle mult;
    public static final MethodHandle div;
    public static final MethodHandle lessThan;
    public static final MethodHandle greaterThan;

    static {
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        MethodType nullaryMethodType = MethodType.methodType(Object.class);
        MethodType unaryMethodType = MethodType.methodType(Object.class, Object.class);
        MethodType binaryMethodType = MethodType.methodType(Object.class, Object.class, Object.class);
        MethodType varargsMethodType = MethodType.methodType(Object.class, Object[].class);

        try {
            car = ProcedureHelpers.getArityMatcher(BuiltIns.procNameCar, 1, lookup.findStatic(BuiltIns.class, "car", unaryMethodType));
            cdr = ProcedureHelpers.getArityMatcher(BuiltIns.procNameCdr, 1, lookup.findStatic(BuiltIns.class, "cdr", unaryMethodType));
            cons = ProcedureHelpers.getArityMatcher(BuiltIns.procNameCons, 2, lookup.findStatic(BuiltIns.class, "cons", binaryMethodType));
            display = ProcedureHelpers.getArityMatcher(BuiltIns.procNameDisplay, 1, lookup.findStatic(BuiltIns.class, "display", unaryMethodType));
            areEq = ProcedureHelpers.getArityMatcher(BuiltIns.procNameEq, 2, lookup.findStatic(BuiltIns.class, "areEq", binaryMethodType));
            toList = ProcedureHelpers.getVarargsMatcher(lookup.findStatic(BuiltIns.class, "toList", varargsMethodType));
            newline = ProcedureHelpers.getArityMatcher(BuiltIns.procNameNewline, 0, lookup.findStatic(BuiltIns.class, "newline", nullaryMethodType));
            not = ProcedureHelpers.getArityMatcher(BuiltIns.procNameNot, 1, lookup.findStatic(BuiltIns.class, "not", unaryMethodType));
            isNull = ProcedureHelpers.getArityMatcher(BuiltIns.procNameNull, 1, lookup.findStatic(BuiltIns.class, "isNull", unaryMethodType));
            isZero = ProcedureHelpers.getArityMatcher(BuiltIns.procNameZero, 1, lookup.findStatic(BuiltIns.class, "isZero", unaryMethodType));
            plus = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "plus", varargsMethodType));
            minus = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "minus", varargsMethodType));
            mult = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "mult", varargsMethodType));
            div = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "div", varargsMethodType));
            lessThan = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "lessThan", varargsMethodType));
            greaterThan = ProcedureHelpers.getBuiltInVarargsMatcher(lookup.findStatic(BuiltIns.class, "greaterThan", varargsMethodType));
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new InternalError(e);
        }
    }
}
