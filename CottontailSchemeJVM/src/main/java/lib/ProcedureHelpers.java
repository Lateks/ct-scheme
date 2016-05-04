package lib;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

public class ProcedureHelpers {

    public static Object matchArity(String procedureName, Integer arity, MethodHandle m, Object[] args) {
        if (args.length != arity)
            throw new ArityMismatchError(procedureName, arity, args.length);

        try {
            return m.asSpreader(Object[].class, arity).invoke(args);
        } catch (Throwable t) {
            throw new InternalError(t);
        }
    }

    public static Object convertVarargs(MethodHandle m, Object[] args) {
        try {
            return m.invoke(BuiltIns.toList(args));
        } catch (Throwable t) {
            throw new InternalError(t);
        }
    }

    public static MethodHandle getArityMatcher(MethodHandle m, String procedureName, int arity) {
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        MethodType mt = MethodType.methodType(Object.class, String.class, Integer.class, MethodHandle.class, Object[].class);

        try {
            MethodHandle mh = lookup.findStatic(ProcedureHelpers.class, "matchArity", mt);
            return mh.bindTo(procedureName).bindTo(arity).bindTo(m).asVarargsCollector(Object[].class);
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new InternalError(e);
        }
    }

    public static MethodHandle getVarargsMatcher(MethodHandle m) {
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        MethodType mt = MethodType.methodType(Object.class, MethodHandle.class, Object[].class);

        try {
            MethodHandle mh = lookup.findStatic(ProcedureHelpers.class, "convertVarargs", mt);
            return mh.bindTo(m).asVarargsCollector(Object[].class);
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new InternalError(e);
        }
    }

    public static MethodHandle getBuiltInVarargsMatcher(MethodHandle m) {
        return m.asVarargsCollector(Object[].class);
    }

}
