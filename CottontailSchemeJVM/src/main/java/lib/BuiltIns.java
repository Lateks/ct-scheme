package lib;

public class BuiltIns {
    public static Object display(Object obj) {
        System.out.println(obj);
        return CTUndefined.instance;
    }
}
