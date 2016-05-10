package lib;

public class CTReferenceCell {
    private Object ref;

    public CTReferenceCell() {}

    public CTReferenceCell(Object ref) {
        this.ref = ref;
    }

    public void set(Object ref) {
        this.ref = ref;
    }

    public Object get() {
        return ref;
    }
}
