package lib;

public class ArityMismatchError extends RuntimeException {
    public ArityMismatchError(String procedureName, String expected, int given) {
        super(procedureName + ": arity mismatch\n\texpected: " + expected + "\n\tgiven: " + given);
    }

    public ArityMismatchError(String procedureName, int expected, int given) {
        this(procedureName, "" + expected, given);
    }
}
