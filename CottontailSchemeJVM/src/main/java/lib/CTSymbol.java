package lib;

// Symbols are implemented as strings for simplicity
// in this prototype implementation.
public class CTSymbol extends CTString {
    public CTSymbol(String s) {
        super(s);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof CTSymbol && super.equals(obj);
    }
}
