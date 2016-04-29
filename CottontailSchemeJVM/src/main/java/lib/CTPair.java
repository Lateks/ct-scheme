package lib;

public class CTPair extends CTObject {
    private static final String typeName = "pair";

    private Object car;
    private Object cdr;

    public CTPair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CTPair) {
            CTPair other = (CTPair)obj;
            return other.car.equals(this.car) && other.cdr.equals(this.cdr);
        } else {
            return false;
        }
    }

    @Override
    public String toString() {
        StringBuilder repr = new StringBuilder("(");
        repr.append(car.toString()); // TODO: procedures?

        Object tail = cdr;
        while (true)
        {
            if (tail instanceof CTPair) {
                CTPair rest = (CTPair)tail;
                repr.append(" ").append(rest.car.toString());
                tail = rest.cdr;
            } else {
                if (!(tail instanceof CTEmptyList))
                    repr.append(" . ").append(tail.toString()); // TODO: procedures?
                break;
            }
        }

        repr.append(")");
        return repr.toString();
    }

    public String getTypeName() {
        return typeName;
    }

    public Object getCar() {
        return car;
    }

    public Object getCdr() {
        return cdr;
    }
}
