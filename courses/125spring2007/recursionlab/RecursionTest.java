package recursionlab;

import myarraylist.List;

public class RecursionTest {
	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		lengthTest();
		sumTest();
		countTest();
		recMinTest();
		recMaxTest();
		expandTest();
		System.out.println("all recursion tests passed");
	}

	private static void expandTest() {
		for (int i = 1; i <= 5; ++i) {
			System.out.printf("expand(%s, %s, %s) = %s\n", "GNU",
					"GNU's Not Unix", i, Recursion.expand("GNU",
							"GNU's Not Unix", i));
		}
	}

	public static void lengthTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.length(arr) == 0;
		arr.add(3);
		assert Recursion.length(arr) == 1;
		arr.add(4);
		assert Recursion.length(arr) == 2;
	}

	public static void sumTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.sum(arr) == 0;
		arr.add(3);
		assert Recursion.sum(arr) == 3;
		arr.add(4);
		assert Recursion.sum(arr) == 7;
	}

	public static void countTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.count(arr, 5) == 0;
		arr.add(7);
		assert Recursion.count(arr, 5) == 0;
		assert Recursion.count(arr, 7) == 1;
		arr.add(8);
		assert Recursion.count(arr, 5) == 0;
		assert Recursion.count(arr, 7) == 1;
		assert Recursion.count(arr, 8) == 1;
		arr.add(7);
		assert Recursion.count(arr, 5) == 0;
		assert Recursion.count(arr, 7) == 2;
		assert Recursion.count(arr, 8) == 1;
		arr.add(7);
		assert Recursion.count(arr, 5) == 0;
		assert Recursion.count(arr, 7) == 3;
		assert Recursion.count(arr, 8) == 1;
	}

	public static void recMinTest() {
		List<Integer> arr = new List<Integer>();
		arr.add(5);
		assert Recursion.recMin(arr) == 5;
		arr.add(3);
		assert Recursion.recMin(arr) == 3;
		arr.add(-7);
		assert Recursion.recMin(arr) == -7;
		arr.add(7);
		assert Recursion.recMin(arr) == -7;
	}

	public static void recMaxTest() {
		List<Integer> arr = new List<Integer>();
		arr.add(5);
		assert Recursion.recMax(arr) == 5;
		arr.add(3);
		assert Recursion.recMax(arr) == 5;
		arr.add(-7);
		assert Recursion.recMax(arr) == 5;
		arr.add(7);
		assert Recursion.recMax(arr) == 7;
	}

	public static void isSortedAscendingTest() {
		List<Integer> arr = new List<Integer>();
		assert Recursion.isSortedAscending(arr);
		arr.add(3);
		assert Recursion.isSortedAscending(arr);
		arr.add(3);
		assert Recursion.isSortedAscending(arr);
		arr.add(4);
		assert Recursion.isSortedAscending(arr);
		arr.add(354);
		assert Recursion.isSortedAscending(arr);
		arr.add(4);
		assert !Recursion.isSortedAscending(arr);
		arr.add(10);
		assert !Recursion.isSortedAscending(arr);
		arr.clear();
		arr.add(10);
		arr.add(5);
		assert !Recursion.isSortedAscending(arr);
		arr.add(6);
		assert !Recursion.isSortedAscending(arr);
	}
}
