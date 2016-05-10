package lib;

public class NotAProcedureError extends RuntimeException {
    public NotAProcedureError(Object obj) {
        super("Not a procedure: " + obj);
    }
}
