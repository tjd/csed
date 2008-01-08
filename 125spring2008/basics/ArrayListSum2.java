package basics;

import java.util.ArrayList;

public class ArrayListSum2 {

    public static void main(String[] args) {
        ArrayList<Double> arr = new ArrayList<Double>();
        arr.add(-54.3);
        arr.add(6.9);
        arr.add(2.12);
        arr.add(0.0); // writing 0 instead of 0.0 causes
        arr.add(41.0); // a compiler error!

        double sum = 0;
        for (Double x : arr) { // "for-each" loop
            sum += x;
        }
        System.out.printf("arr = %s\nsum = %.2f", arr.toString(), sum);

    }

}
