package lib;

public class CTString extends CTObject {
    private static final String typeName = "string";

    private String s = null;

    public CTString(String s) {
        this.s = s;
    }

    public String getTypeName() {
        return typeName;
    }

    @Override
    public String toString() {
        return s;
    }
}
