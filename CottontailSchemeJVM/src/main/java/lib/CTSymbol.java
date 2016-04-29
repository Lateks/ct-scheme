package lib;

// Symbols are implemented as strings for simplicity
// in this prototype implementation.
public class CTSymbol extends CTString {
    private static final String typeName = "symbol";

    public CTSymbol(String s) {
        super(s);
    }

    @Override
    public String getTypeName() {
        return typeName;
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof CTSymbol && super.equals(obj);
    }
}
