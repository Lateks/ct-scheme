package lib;

public class ProcedureHelpers {
    private static void checkArgs(String procedureName, int expectedArity, Object[] args) {
        if (args.length != expectedArity)
            throw new ArityMismatchError(procedureName, expectedArity, args.length);
    }

    public static Object match0(CTProcedure0 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 0, args);
        return proc.apply();
    }

    public static Object match1(CTProcedure1 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 1, args);
        return proc.apply(args[0]);
    }

    public static Object match2(CTProcedure2 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 2, args);
        return proc.apply(args[0], args[1]);
    }

    public static Object match3(CTProcedure3 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 3, args);
        return proc.apply(args[0], args[1], args[2]);
    }

    public static Object match4(CTProcedure4 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 4, args);
        return proc.apply(args[0], args[1], args[2], args[3]);
    }

    public static Object match5(CTProcedure5 proc, String procedureName, Object[] args) {
        checkArgs(procedureName, 5, args);
        return proc.apply(args[0], args[1], args[2], args[3], args[4]);
    }

    public static Object matchVarargs(CTProcedure1 proc, Object[] args) {
        return proc.apply(BuiltIns.toList(args));
    }

    public static CTProcedure match0(CTProcedure0 proc, String procedureName) {
        return (Object[] args) -> match0(proc, procedureName, args);
    }

    public static CTProcedure match1(CTProcedure1 proc, String procedureName) {
        return (Object[] args) -> match1(proc, procedureName, args);
    }

    public static CTProcedure match2(CTProcedure2 proc, String procedureName) {
        return (Object[] args) -> match2(proc, procedureName, args);
    }

    public static CTProcedure match3(CTProcedure3 proc, String procedureName) {
        return (Object[] args) -> match3(proc, procedureName, args);
    }

    public static CTProcedure match4(CTProcedure4 proc, String procedureName) {
        return (Object[] args) -> match4(proc, procedureName, args);
    }

    public static CTProcedure match5(CTProcedure5 proc, String procedureName) {
        return (Object[] args) -> match5(proc, procedureName, args);
    }

    public static CTProcedure matchVarargs(CTProcedure1 proc) {
        return (Object[] args) -> matchVarargs(proc, args);
    }

    public static Object callProcedure(Object p, Object[] args) {
        if (p instanceof CTProcedure) {
            return ((CTProcedure) p).apply(args);
        } else {
            throw new NotAProcedureError(p);
        }
    }

    public static Object trampoline(Object v) {
        Object currentValue = v;
        while (currentValue instanceof CTTailContinuation) {
            currentValue = ((CTTailContinuation) currentValue).value.apply();
        }
        return currentValue;
    }
}
