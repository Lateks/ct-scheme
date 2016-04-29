package lib;

public class CTBool extends CTObject {
    public static final CTObject trueInstance = new CTBool(true);
    public static final CTObject falseInstance = new CTBool(false);

    public static final String typeName = "boolean";
    private boolean value;

    private CTBool(boolean value) {
        this.value = value;
    }

    public static CTObject toCTBool(boolean b) {
        if (b) {
            return trueInstance;
        } else {
            return falseInstance;
        }
    }

    @Override
    public String toString() {
        if (value) {
            return "#t";
        } else {
            return "#f";
        }
    }

    public String getTypeName() {
        return typeName;
    }
}
