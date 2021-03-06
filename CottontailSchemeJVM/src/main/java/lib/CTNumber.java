package lib;

public class CTNumber extends CTObject {
    public static final String typeName = "number";

    public final double value;

    public CTNumber(double d) {
        value = d;
    }

    @Override
    public String toString() {
        int valueAsInt = (int)value;
        double fraction = value - valueAsInt;
        if (fraction > 0) {
            return "" + value;
        } else {
            return "" + valueAsInt;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        } else if (obj instanceof CTNumber) {
            CTNumber other = (CTNumber)obj;
            return other.value == this.value;
        } else {
            return false;
        }
    }

    public String getTypeName() {
        return typeName;
    }
}
