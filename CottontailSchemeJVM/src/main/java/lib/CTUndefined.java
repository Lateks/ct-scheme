package lib;

public class CTUndefined extends CTObject {
    public static final CTObject instance = new CTUndefined();

    private CTUndefined() {}

    public static final String typeName = "undefined";

    public String getTypeName() {
        return typeName;
    }

    @Override
    public String toString() {
        return "";
    }
}
