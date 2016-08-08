package lib;

public class CTString extends CTObject {
    public static final String typeName = "string";

    public final String value;

    public CTString(String value) {
        this.value = value;
    }

    public String getTypeName() {
        return typeName;
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj instanceof CTString) {
            CTString other = (CTString)obj;
            return other.value.equals(this.value);
        } else {
            return false;
        }
    }
}
