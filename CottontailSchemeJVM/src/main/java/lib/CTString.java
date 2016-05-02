package lib;

public class CTString extends CTObject {
    public static final String typeName = "string";

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

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj instanceof CTString) {
            CTString other = (CTString)obj;
            return other.s.equals(this.s);
        } else {
            return false;
        }
    }
}
