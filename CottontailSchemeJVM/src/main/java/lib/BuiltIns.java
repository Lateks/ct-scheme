package lib;

public class BuiltIns {
    public static Object display(Object obj) {
        System.out.print(obj);
        return CTUndefined.instance;
    }

    public static Object newline() {
        System.out.println();
        return CTUndefined.instance;
    }
}
