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
            car = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "car", unaryMethodType), BuiltIns.procNameCar, 1);
            cdr = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "cdr", unaryMethodType), BuiltIns.procNameCdr, 1);
            cons = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "cons", binaryMethodType), BuiltIns.procNameCons, 2);
            display = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "display", unaryMethodType), BuiltIns.procNameDisplay, 1);
            areEq = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "areEq", binaryMethodType), BuiltIns.procNameEq, 2);
            toList = ProcedureHelpers.getVarargsMatcher(lookup.findStatic(BuiltIns.class, "toList", varargsMethodType));
            newline = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "newline", nullaryMethodType), BuiltIns.procNameNewline, 0);
            not = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "not", unaryMethodType), BuiltIns.procNameNot, 1);
            isNull = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "isNull", unaryMethodType), BuiltIns.procNameNull, 1);
            isZero = ProcedureHelpers.getArityMatcher(lookup.findStatic(BuiltIns.class, "isZero", unaryMethodType), BuiltIns.procNameZero, 1);
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
