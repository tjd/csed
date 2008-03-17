package binarysearch;

public class BinarySearch {

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		test1();
		test2();
		System.out.printf("All tests done.\n");
	}

	private static void test2() {
		assert recBinarySearch(3, new int[] {}) == -1;
		assert recBinarySearch(3, new int[] { 3 }) == 0;
		assert recBinarySearch(3, new int[] { 5 }) == -1;
		assert recBinarySearch(3, new int[] { 3, 5 }) == 0;
		assert recBinarySearch(5, new int[] { 3, 5 }) == 1;
		assert recBinarySearch(2, new int[] { 3, 5 }) == -1;
		assert recBinarySearch(4, new int[] { 3, 5 }) == -1;
		assert recBinarySearch(6, new int[] { 3, 5 }) == -1;
		assert recBinarySearch(3, new int[] { 3, 5, 6 }) == 0;
		assert recBinarySearch(5, new int[] { 3, 5, 6 }) == 1;
		assert recBinarySearch(6, new int[] { 3, 5, 6 }) == 2;
		assert recBinarySearch(2, new int[] { 3, 5, 6 }) == -1;
		assert recBinarySearch(4, new int[] { 3, 5, 6 }) == -1;
		assert recBinarySearch(9, new int[] { 3, 5, 6 }) == -1;
		assert recBinarySearch(9, new int[] { 3, 5, 6, 7, 8, 9, 22, 100 }) == 5;
		assert recBinarySearch(100, new int[] { 3, 5, 6, 7, 8, 9, 22, 100 }) == 7;
		System.out.printf("All test2 tests done.\n");
	}

	private static void test1() {
		assert binarySearch(3, new int[] {}) == -1;
		assert binarySearch(3, new int[] { 3 }) == 0;
		assert binarySearch(3, new int[] { 5 }) == -1;
		assert binarySearch(3, new int[] { 3, 5 }) == 0;
		assert binarySearch(5, new int[] { 3, 5 }) == 1;
		assert binarySearch(2, new int[] { 3, 5 }) == -1;
		assert binarySearch(4, new int[] { 3, 5 }) == -1;
		assert binarySearch(6, new int[] { 3, 5 }) == -1;
		assert binarySearch(3, new int[] { 3, 5, 6 }) == 0;
		assert binarySearch(5, new int[] { 3, 5, 6 }) == 1;
		assert binarySearch(6, new int[] { 3, 5, 6 }) == 2;
		assert binarySearch(2, new int[] { 3, 5, 6 }) == -1;
		assert binarySearch(4, new int[] { 3, 5, 6 }) == -1;
		assert binarySearch(9, new int[] { 3, 5, 6 }) == -1;
		assert binarySearch(9, new int[] { 3, 5, 6, 7, 8, 9, 22, 100 }) == 5;
		assert binarySearch(100, new int[] { 3, 5, 6, 7, 8, 9, 22, 100 }) == 7;
		System.out.printf("All test1 tests done.\n");
	}

	// Assumes arr is in ascending sorted order.
	public static int binarySearch(int x, int[] arr) {
		return binarySearch(x, arr, 0, arr.length - 1);
	}

	public static int binarySearch(int x, int[] arr, int begin, int end) {
		while (begin <= end) {
			int mid = (begin + end) / 2;
			if (arr[mid] == x) {
				return mid;
			} else if (x < arr[mid]) {
				end = mid - 1;
			} else {
				begin = mid + 1;
			}
		}
		return -1;
	}

	// Assumes arr is in ascending sorted order.
	public static int recBinarySearch(int x, int[] arr) {
		return recBinarySearch(x, arr, 0, arr.length - 1);
	}

	public static int recBinarySearch(int x, int[] arr, int begin, int end) {
		if (begin > end) { // base case
			return -1;
		} else {
			int mid = (begin + end) / 2;
			if (x == arr[mid]) {
				return mid;
			} else if (x < arr[mid]) {
				return recBinarySearch(x, arr, begin, mid - 1);
			} else {
				return recBinarySearch(x, arr, mid + 1, end);
			}
		}
	}

}
