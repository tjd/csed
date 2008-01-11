package basics;

import java.util.ArrayList;

public class ArrayListSum1 {

    public static void main(String[] args) {
        ArrayList<Double> arr = new ArrayList<Double>();
        arr.add(-54.3);
        arr.add(6.9);
        arr.add(2.12);
        arr.add(0.0); // writing 0 instead of 0.0 causes
        arr.add(41.0); // a compiler error!

        double sum = 0;
        for (int i = 0; i < arr.size(); ++i) {
            sum += arr.get(i);
        }
        System.out.printf("arr = %s\nsum = %.2f", arr.toString(), sum);
    }

}
