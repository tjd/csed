package basics;

/* 
 * Demonstates variable-length argument lists.
 * 
 */

import java.util.ArrayList;

public class Varargs {

    public static void main(String[] args) {
        System.out.printf("sum 1 = %s\n", sum(5, 3, 2, 6));
        System.out.printf("sum 2 = %s\n", sum(0, -5, -33, 23, 4, 7, 6, 4));

        ArrayList<String> arr1 = makeArrayList("me", "and", "my", "monkey");
        ArrayList<String> arr2 = makeArrayList("up", "down", "all", "around",
                "twice", "through", "the", "middle");

        System.out.printf("ArrayList 1 = %s\n", arr1);
        System.out.printf("ArrayList 2 = %s\n", arr2);
    }

    public static int sum(int... nums) {
        int result = 0;
        for (int i = 0; i < nums.length; ++i) {
            result += nums[i];
        }
        return result;
    }

    public static ArrayList<String> makeArrayList(String... vals) {
        ArrayList<String> result = new ArrayList<String>();
        for (int i = 0; i < vals.length; ++i) {
            result.add(vals[i]);
        }
        return result;
    }

}
