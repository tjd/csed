package stats;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;

/*
 * 
 * Basic desriptive statistics.
 * 
 */

public class Descriptive {

	/*
	 * Ideally, the sum function should return a value of type T. But that
	 * is impossible because the Number class does not support addition or
	 * zero-assignment. A Number object can only convert itself to the six
	 * primitive numeric types. Using double everywhere is inelegant, but
	 * practical since so many real-world applications use floating-point
	 * numbers.
	 */
	public static <T extends Number> double sum(Collection<T> arr) {
		double total = 0;
		for(T n : arr) {
			total += n.doubleValue();
		}
		return total;
	}
	
	public static double mean(Collection<? extends Number> arr) {
		return sum(arr) / arr.size();
	}
	
	public static <T extends Number> double variance(Collection<T> arr) {
		double total = 0;
		for(T n : arr) {
			double d = n.doubleValue();
			total += d * d;
		}
		return total / (arr.size() - 1);
	}
	
	public static double std_dev(Collection<? extends Number> arr) {
		return Math.sqrt(variance(arr));
	}

	public static void meanTest() {
		ArrayList<Double> arr1 = new ArrayList<Double>();
		arr1.add(1.0);
		System.out.printf("mean(%s) = %s\n", arr1, mean(arr1));
		arr1.add(2.0);
		System.out.printf("mean(%s) = %s\n", arr1, mean(arr1));
	}

	public static void main(String[] args) {
		meanTest();
	}

}
