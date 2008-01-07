package basics;

public class ArithmeticCasting {

    public static void main(String[] args) {
        int x = (int) 2.1332; // explicit (int) cast required

        double y = 45; // no explicit cast required

        System.out.printf("x = %s, y = %.2f\n", x, y);

        int a = 25;
        long b = a; // no explicit cast required

        long c = 25;
        int d = (int) c; // explicit (int) cast required
        System.out.printf("a = %s, b = %s,  c = %s, d = %s\n", a, b, c, d);
    }

}
