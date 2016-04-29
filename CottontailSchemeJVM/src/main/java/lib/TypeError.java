package lib;

public class TypeError extends RuntimeException {

    public TypeError(String procedureName, String expectedType, String receivedType) {
        super(procedureName + ": contract violation\n\texpected: " + expectedType + "\n\tgiven: " + receivedType);
    }
}
