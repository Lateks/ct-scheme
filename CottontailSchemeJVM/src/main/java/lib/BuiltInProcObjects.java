package lib;

public class BuiltInProcObjects {
    public static final CTProcedure car;
    public static final CTProcedure cdr;
    public static final CTProcedure cons;
    public static final CTProcedure display;
    public static final CTProcedure areEq;
    public static final CTProcedure toList;
    public static final CTProcedure newline;
    public static final CTProcedure not;
    public static final CTProcedure isNull;
    public static final CTProcedure isZero;
    public static final CTProcedure plus;
    public static final CTProcedure minus;
    public static final CTProcedure mult;
    public static final CTProcedure div;
    public static final CTProcedure lessThan;
    public static final CTProcedure greaterThan;

    static {
        car = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameCar, BuiltIns::car, args);
        cdr = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameCdr, BuiltIns::cdr, args);
        cons = (Object[] args) -> ProcedureHelpers.match2(BuiltIns.procNameCons, BuiltIns::cons, args);
        display = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameDisplay, BuiltIns::display, args);
        areEq = (Object[] args) -> ProcedureHelpers.match2(BuiltIns.procNameEq, BuiltIns::areEq, args);
        toList = BuiltIns::toList;
        newline = (Object[] args) -> ProcedureHelpers.match0(BuiltIns.procNameNewline, BuiltIns::newline, args);
        not = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameNot, BuiltIns::not, args);
        isNull = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameNull, BuiltIns::isNull, args);
        isZero = (Object[] args) -> ProcedureHelpers.match1(BuiltIns.procNameZero, BuiltIns::isZero, args);
        plus = BuiltIns::plus;
        minus = BuiltIns::minus;
        mult = BuiltIns::mult;
        div = BuiltIns::div;
        lessThan = BuiltIns::lessThan;
        greaterThan = BuiltIns::greaterThan;
    }
}
