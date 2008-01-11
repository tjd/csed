package basics;

public class ArraySum {

    public static void main(String[] args) {
        double[] arr = { -54.3, 6.9, 2.12, 0, 41 };
        double sum = 0;
        for (int i = 0; i < arr.length; ++i) {
            sum += arr[i]; // short for: sum = sum + arr[i]
        }
        System.out.printf("sum = %.2f", sum);
    }
}
