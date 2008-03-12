package linearSearch;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

import simplelist.List;
import util.ArrayList125;

public class LinearSearch {

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		// test1();
		// test2();
		// test3();
		// test4();
		// test5();
		// test6();
		// test7();
		// test8();
		// test9();
		// test10();
		// test11();
		// test12();
		// test13();
		test14();
		System.out.printf("-- All tests passed\n");
	}

	private static Random rnd = new Random();

	private static ArrayList<Integer> makeRandomArray(int size) {
		ArrayList<Integer> arr = new ArrayList<Integer>(size);
		for (int i = 0; i < size; ++i) {
			arr.add(rnd.nextInt(1000));
		}
		return arr;
	}

	private static int sum(ArrayList<Integer> arr) {
		int result = 0;
		for (Integer x : arr) {
			result += x;
		}
		return result;
	}

	private static void test14() {
		ArrayList<Integer> counts = new ArrayList<Integer>();
		for (int i = 0; i < 10000; ++i) {
			ArrayList<Integer> arr = makeRandomArray(1000);		
			if (rnd.nextBoolean()) { // 50/50 chance that 5555 is in the list
				arr.add(rnd.nextInt(arr.size()), 5555);
			}

			int count = countedLinearSearch(5555, arr);
			counts.add(count);
		}
		System.out.printf("Average number of comparisons: %.2f\n", sum(counts)
				/ ((double) counts.size()));
	}

	private static void test12() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert sentinelLinearSearch(3, arr) == true;
		assert sentinelLinearSearch(5, arr) == true;
		assert sentinelLinearSearch(9, arr) == true;
		assert sentinelLinearSearch(2, arr) == false;
		assert sentinelLinearSearch(6, arr) == false;
		assert sentinelLinearSearch(10, arr) == false;
		System.out.printf("test12 done\n");
	}

	private static void test11() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch8(3, arr) == true;
		assert linearSearch8(5, arr) == true;
		assert linearSearch8(9, arr) == true;
		assert linearSearch8(2, arr) == false;
		assert linearSearch8(6, arr) == false;
		assert linearSearch8(10, arr) == false;
		System.out.printf("test11 done\n");
	}

	private static void test10() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 5, 5, 9,
				7, 8, 9);
		assert count(3, arr) == 1;
		assert count(5, arr) == 3;
		assert count(9, arr) == 2;
		assert count(2, arr) == 0;
		assert count(6, arr) == 0;
		assert count(10, arr) == 0;
		System.out.printf("test10 done\n");
	}

	private static void test9() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch7(3, arr) == true;
		assert linearSearch7(5, arr) == true;
		assert linearSearch7(9, arr) == true;
		assert linearSearch7(2, arr) == false;
		assert linearSearch7(6, arr) == false;
		assert linearSearch7(10, arr) == false;
		System.out.printf("test9 done\n");
	}

	private static void test13() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(3, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(5, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(9, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(2, arr) == false;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(6, arr) == false;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSearch(10, arr) == false;
		System.out.printf("arr = %s\n", arr);
		System.out.printf("test13 done\n");
	}

	private static void test8() {
		ArrayList<Integer> arr = new ArrayList<Integer>();
		for (int i = 0; i < 1000; ++i) {
			arr.add(i);
		}

		profiledLinearSearch(1, arr);
		profiledLinearSearch(999, arr);
		profiledLinearSearch(5000, arr);
	}

	private static void test7() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert profiledLinearSearch(3, arr) == true;
		assert profiledLinearSearch(5, arr) == true;
		assert profiledLinearSearch(9, arr) == true;
		assert profiledLinearSearch(2, arr) == false;
		assert profiledLinearSearch(6, arr) == false;
		assert profiledLinearSearch(10, arr) == false;
		System.out.printf("test7 done\n");
	}

	private static void test6() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert expandingLinearSearch(3, 0, arr) == true;
		assert expandingLinearSearch(5, 1, arr) == true;
		assert expandingLinearSearch(9, 2, arr) == true;
		assert expandingLinearSearch(2, 3, arr) == false;
		assert expandingLinearSearch(6, 4, arr) == false;
		assert expandingLinearSearch(10, 5, arr) == false;
		System.out.printf("test6 done\n");
	}

	private static void test5() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch5(3, arr) == true;
		assert linearSearch5(5, arr) == true;
		assert linearSearch5(9, arr) == true;
		assert linearSearch5(2, arr) == false;
		assert linearSearch5(6, arr) == false;
		assert linearSearch5(10, arr) == false;
		System.out.printf("test5 done\n");
	}

	private static void test4() {
		List arr = List.empty.push(9).push(8).push(7).push(5).push(4).push(3);
		assert linearSearch(3, arr) == true;
		assert linearSearch(5, arr) == true;
		assert linearSearch(9, arr) == true;
		assert linearSearch(2, arr) == false;
		assert linearSearch(6, arr) == false;
		assert linearSearch(10, arr) == false;
		System.out.printf("test4 done\n");

	}

	private static void test3() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch3(3, arr) == true;
		assert linearSearch3(5, arr) == true;
		assert linearSearch3(9, arr) == true;
		assert linearSearch3(2, arr) == false;
		assert linearSearch3(6, arr) == false;
		assert linearSearch3(10, arr) == false;
		System.out.printf("test3 done\n");
	}

	private static void test2() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch2(3, arr) == true;
		assert linearSearch2(5, arr) == true;
		assert linearSearch2(9, arr) == true;
		assert linearSearch2(2, arr) == false;
		assert linearSearch2(6, arr) == false;
		assert linearSearch2(10, arr) == false;
		System.out.printf("test2 done\n");
	}

	private static void test1() {
		ArrayList125<Integer> arr = new ArrayList125<Integer>(3, 4, 5, 7, 8, 9);
		assert linearSearch1(3, arr) == true;
		assert linearSearch1(5, arr) == true;
		assert linearSearch1(9, arr) == true;
		assert linearSearch1(2, arr) == false;
		assert linearSearch1(6, arr) == false;
		assert linearSearch1(10, arr) == false;
		System.out.printf("test1 done\n");
	}

	public static boolean linearSearch1(Integer x, ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				return true;
			}
		}
		return false;
	}

	public static boolean linearSearch2(Integer x, ArrayList<Integer> arr) {
		for (Integer n : arr) {
			if (n.equals(x)) {
				return true;
			}
		}
		return false;
	}

	public static boolean linearSearch3(Integer x, ArrayList<Integer> arr) {
		int i = 0;
		while (i < arr.size()) {
			if (arr.get(i).equals(x)) {
				return true;
			}
			i = i + 1;
		}
		return false;
	}

	public static boolean linearSearch4(Integer x, ArrayList<Integer> arr) {
		for (int i = arr.size(); i >= 0; --i) {
			if (arr.get(i).equals(x)) {
				return true;
			}
		}
		return false;
	}

	public static boolean linearSearch(int x, List lst) {
		if (lst.isEmpty()) {
			return false;
		} else if (lst.first() == x) {
			return true;
		} else {
			return linearSearch(x, lst.rest());
		}
	}

	public static boolean linearSearch5(Integer x, ArrayList<Integer> arr) {
		return linearSearch5(x, 0, arr);
	}

	private static boolean linearSearch5(Integer x, int start,
			ArrayList<Integer> arr) {
		int n = arr.size();
		if (start >= n) {
			return false;
		} else if (arr.get(start).equals(x)) {
			return true;
		} else {
			return linearSearch5(x, start + 1, arr);
		}
	}

	public static boolean expandingLinearSearch(Integer x, int hint,
			ArrayList<Integer> arr) {
		final int n = arr.size();
		int left = hint;
		int right = hint + 1;
		// for (int count = 0; count < n; ++count) {
		while (left >= 0 || right < n) {
			if (left >= 0 && arr.get(left).equals(x)) {
				return true;
			}
			if (right < n && arr.get(right).equals(x)) {
				return true;
			}
			--left;
			++right;
		}
		return false;
	}

	public static boolean sentinelLinearSearch(Integer x, ArrayList<Integer> arr) {
		final int n = arr.size();
		final Integer last = arr.get(n - 1);
		if (last.equals(x)) {
			return true;
		} else {
			arr.set(n - 1, x);
			int i = 0;
			while (!arr.get(i).equals(x)) {
				++i;
			}
			arr.set(n - 1, last);
			if (i == n - 1) {
				return false;
			} else {
				return true;
			}
		}
	}

	public static boolean profiledLinearSearch(Integer x, ArrayList<Integer> arr) {
		int c1 = 0, c2 = 0, c3 = 0, c4 = 0, c5 = 0, c6 = 0;

		++c1;
		int i = 0;

		++c2;
		while (i < arr.size()) {
			++c3;
			if (arr.get(i).equals(x)) {
				++c4;
				System.out
						.printf(
								"c1 = %s, c2 = %s, c3 = %s, c4 = %s, c5 = %s, c6 = %s\n",
								c1, c2, c3, c4, c5, c6);
				return true;
			}

			++c5;
			i = i + 1;

			++c2;
		}
		++c6;
		System.out.printf(
				"c1 = %s, c2 = %s, c3 = %s, c4 = %s, c5 = %s, c6 = %s\n", c1,
				c2, c3, c4, c5, c6);
		return false;
	}

	public static int countedLinearSearch(Integer x, ArrayList<Integer> arr) {
		int count = 0;
		for (int i = 0; i < arr.size(); ++i) {
			++count;
			if (arr.get(i).equals(x)) {
				return count;
			}
		}
		return count;
	}

	public static int indexOf(Integer x, ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				return i;
			}
		}
		return -1;
	}

	public static boolean linearSearch7(Integer x, ArrayList<Integer> arr) {
		return indexOf(x, arr) != -1;
	}

	public static int count(Integer x, ArrayList<Integer> arr) {
		int c = 0;
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				c = c + 1;
			}
		}
		return c;
	}

	public static boolean linearSearch8(Integer x, ArrayList<Integer> arr) {
		return count(x, arr) > 0;
	}

	public static boolean selfModifyingLinearSearch(int x,
			ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				// swap the item at i with the first item of the list
				int temp = arr.get(0);
				arr.set(0, arr.get(i));
				arr.set(i, temp);
				return true;
			}
		}
		return false;
	}
}
