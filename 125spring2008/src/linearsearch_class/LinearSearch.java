package linearsearch_class;

import java.util.ArrayList;

import simplelist.List;
import util.ArrayList125;

public class LinearSearch {

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		test1();
		test2();
		test3();
		test4();
		test5();
		test6();
		test7();

		System.out.printf("All tests done!\n");
	}

	private static void test7() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(2, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(7, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(8, arr) == true;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(-1, arr) == false;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(5, arr) == false;
		System.out.printf("arr = %s\n", arr);
		assert selfModifyingLinearSeaching(10, arr) == false;
		System.out.printf("arr = %s\n", arr);
		System.out.printf("test7 done\n");
	}

	private static void test6() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		assert expandingLinearSearch(2, arr, 0) == true;
		assert expandingLinearSearch(7, arr, 1) == true;
		assert expandingLinearSearch(8, arr, 3) == true;
		assert expandingLinearSearch(-1, arr, 5) == false;
		assert expandingLinearSearch(5, arr, 2) == false;
		assert expandingLinearSearch(10, arr, 1) == false;
		System.out.printf("test6 done\n");
	}

	private static void test5() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		assert linearSearch5(2, arr) == true;
		assert linearSearch5(7, arr) == true;
		assert linearSearch5(8, arr) == true;
		assert linearSearch5(-1, arr) == false;
		assert linearSearch5(5, arr) == false;
		assert linearSearch5(10, arr) == false;
		System.out.printf("test5 done\n");
	}

	private static void test4() {
		// ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		List arr = List.empty.push(8).push(4).push(6).push(7).push(3).push(1)
				.push(2);
		assert linearSearch4(2, arr) == true;
		assert linearSearch4(7, arr) == true;
		assert linearSearch4(8, arr) == true;
		assert linearSearch4(-1, arr) == false;
		assert linearSearch4(5, arr) == false;
		assert linearSearch4(10, arr) == false;
		System.out.printf("test4 done\n");
	}

	private static void test3() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		assert linearSearch3(2, arr) == true;
		assert linearSearch3(7, arr) == true;
		assert linearSearch3(8, arr) == true;
		assert linearSearch3(-1, arr) == false;
		assert linearSearch3(5, arr) == false;
		assert linearSearch3(10, arr) == false;
		System.out.printf("test3 done\n");

	}

	private static void test2() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		assert linearSearch2(2, arr) == true;
		assert linearSearch2(7, arr) == true;
		assert linearSearch2(8, arr) == true;
		assert linearSearch2(-1, arr) == false;
		assert linearSearch2(5, arr) == false;
		assert linearSearch2(10, arr) == false;
		System.out.printf("test2 done\n");
	}

	private static void test1() {
		ArrayList125<Integer> arr = new ArrayList125(2, 1, 3, 7, 6, 4, 8);
		assert linearSearch1(2, arr) == true;
		assert linearSearch1(7, arr) == true;
		assert linearSearch1(8, arr) == true;
		assert linearSearch1(-1, arr) == false;
		assert linearSearch1(5, arr) == false;
		assert linearSearch1(10, arr) == false;
		System.out.printf("test1 done\n");
	}

	public static boolean linearSearch1(int x, ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				return true;
			}
		} // for
		return false;
	}

	public static boolean linearSearch2(int x, ArrayList<Integer> arr) {
		int i = 0;
		final int n = arr.size();
		while (i < n) {
			if (arr.get(i).equals(x)) {
				return true;
			}
			++i;
		}
		return false;
	}

	public static boolean sentinelLinearSearch(int x, ArrayList<Integer> arr) {
		final int n = arr.size();
		Integer last = arr.get(n - 1);
		if (last.equals(x)) {
			return true;
		} else {
			arr.set(n - 1, x);
			int i = 0;
			while (!arr.get(i).equals(x)) {
				++i;
			}
			arr.set(n - 1, last);
			return i < n - 1;
//			if (i == n - 1) {
//				return false;
//			} else {
//				return true;
//			}
		}
	}

	public static boolean linearSearch3(int x, ArrayList<Integer> arr) {
		for (int i = arr.size() - 1; i >= 0; --i) {
			if (arr.get(i).equals(x)) {
				return true;
			}
		} // for
		return false;
	}

	public static boolean linearSearch4(int x, List lst) {
		if (lst.isEmpty())
			return false;
		else if (lst.first() == x)
			return true;
		else
			return linearSearch4(x, lst.rest());
	}

	public static boolean linearSearch5(int x, ArrayList<Integer> arr, int start) {
		if (start >= arr.size()) {
			return false;
		} else if (arr.get(start).equals(x)) {
			return true;
		} else {
			return linearSearch5(x, arr, start + 1);
		}
	}

	public static boolean linearSearch5(int x, ArrayList<Integer> arr) {
		return linearSearch5(x, arr, 0);
	}

	public static boolean expandingLinearSearch(int x, ArrayList<Integer> arr,
			int hint) {
		int n = arr.size();
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

	public static boolean selfModifyingLinearSeaching(int x,
			ArrayList<Integer> arr) {
		for (int i = 0; i < arr.size(); ++i) {
			if (arr.get(i).equals(x)) {
				// swap arr(i) with arr(0)
				int temp = arr.get(0);
				arr.set(0, arr.get(i));
				arr.set(i, temp);
				return true;
			}
		} // for
		return false;
	}

}
