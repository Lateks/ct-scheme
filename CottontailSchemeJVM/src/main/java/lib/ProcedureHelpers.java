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

}