package lib;

public class InternalError extends RuntimeException {

    public InternalError(Throwable t) {
        super("Internal error occurred: " + t);
    }
}
