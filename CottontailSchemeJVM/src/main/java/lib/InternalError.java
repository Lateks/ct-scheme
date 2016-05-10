package lib;

public class InternalError extends RuntimeException {

    public InternalError(Throwable t) {
        this(t.toString());
    }

    public InternalError(String s) {
        super("Internal error occurred: " + s);
    }
}
