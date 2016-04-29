package lib;

public class CTEmptyList extends CTObject {
    public static final String typeName = "()";

    public static CTObject instance = new CTEmptyList();

    private CTEmptyList() {}

    public String getTypeName() {
        return typeName;
    }

    @Override
    public String toString() {
        return "()";
    }
}
