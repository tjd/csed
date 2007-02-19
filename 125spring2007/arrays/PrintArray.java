package arrays;

public class PrintArray {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		testPrint();
		sum1Test();
		indexOfMinTest();
		minTest();
		copyTest();
		reverseInPlaceTest();
		reverseCopyTest();
		System.out.println("All array tests passed!!");
	}

	public static void print1(int[] arr) {
		for (int i = 0; i < arr.length; ++i) {
			System.out.print(arr[i] + " ");
		}
		System.out.println();
	}

	public static void print2(int[] arr) {
		int n = arr.length;
		if (n == 0) {
			System.out.println("{}");
		} else if (n == 1) {
			System.out.println("{" + arr[0] + "}");
		} else {
			System.out.print("{" + arr[0]);
			for (int i = 1; i < n; ++i) {
				System.out.print(", " + arr[i]);
			}
			System.out.println("}");
		}
	}

	public static void testPrint() {
		int[] score = { 98, 90, 85, 86, 44, 100 };
		print1(score);
		print2(score);
	}

	public static int sum1(int[] arr) {
		int result = 0;
		for (int i = 0; i < arr.length; ++i) {
			result += arr[i];
		}
		return result;
	}

	public static int sum2(int[] arr) {
		int result = 0;
		for (Integer n : arr) {
			result += n;
		}
		return result;
	}

	public static void sum1Test() {
		assert sum1(new int[] {}) == 0;
		assert sum1(new int[] { 43 }) == 43;
		assert sum1(new int[] { 2, 8 }) == 10;
		assert sum1(new int[] { -1, -1, 2 }) == 0;
		assert sum1(new int[] { 1, 2, 3, 4 }) == 10;
	}

	public static void sum2Test() {
		assert sum2(new int[] {}) == 0;
		assert sum2(new int[] { 43 }) == 43;
		assert sum2(new int[] { 2, 8 }) == 10;
		assert sum2(new int[] { -1, -1, 2 }) == 0;
		assert sum2(new int[] { 1, 2, 3, 4 }) == 10;
	}

	public static int indexOfMin(int[] arr) {
		int p = 0;
		for (int i = 1; i < arr.length; ++i) {
			if (arr[i] < arr[p]) {
				p = i;
			}
		}
		return p;
	}

	public static int min(int[] arr) {
		return arr[indexOfMin(arr)];
	}

	public static void indexOfMinTest() {
		assert indexOfMin(new int[] { 43 }) == 0;
		assert indexOfMin(new int[] { 2, 8 }) == 0;
		assert indexOfMin(new int[] { 8, 2 }) == 1;
		assert indexOfMin(new int[] { 1, 2, 3, 4 }) == 0;
		assert indexOfMin(new int[] { 1, -2, 3, 4 }) == 1;
		assert indexOfMin(new int[] { 1, 2, -3, 4 }) == 2;
		assert indexOfMin(new int[] { 1, 2, 3, -4 }) == 3;
	}

	public static void minTest() {
		assert min(new int[] { 43 }) == 43;
		assert min(new int[] { 2, 8 }) == 2;
		assert min(new int[] { 8, 2 }) == 2;
		assert min(new int[] { 1, 2, 3, 4 }) == 1;
		assert min(new int[] { 1, -2, 3, 4 }) == -2;
		assert min(new int[] { 1, 2, -3, 4 }) == -3;
		assert min(new int[] { 1, 2, 3, -4 }) == -4;
	}

	public static boolean same(int[] A, int[] B) {
		if (A.length != B.length) {
			return false;
		} else {
			for (int i = 0; i < A.length; ++i) {
				if (A[i] != B[i]) {
					return false;
				}
			}
			return true;
		}
	}

	public static int[] copy(int[] arr) {
		int[] result = new int[arr.length];
		for (int i = 0; i < arr.length; ++i) {
			result[i] = arr[i];
		}
		return result;
	}

	public static void copyTest() {
		assert same(new int[] {}, copy(new int[] {}));
		assert same(new int[] { 4 }, copy(new int[] { 4 }));
		assert same(new int[] { 8, 6 }, copy(new int[] { 8, 6 }));
		assert same(new int[] { 5, -1, 2 }, copy(new int[] { 5, -1, 2 }));
		int[] arr = { 1, 2, 3, 4 };
		int[] cp = copy(arr);
		cp[0] = -9;
		assert arr[0] == 1;
	}

	public static void reverseInPlace(int[] arr) {
		int a = 0;
		int b = arr.length - 1;
		while (a < b) {
			// swap a and b items
			int temp = arr[a];
			arr[a] = arr[b];
			arr[b] = temp;
			++a;
			--b;
		}
	}

	public static void reverseInPlaceTest() {
		int[] arr = {};
		reverseInPlace(arr);
		assert same(arr, new int[] {});
		arr = new int[] { 5 };
		reverseInPlace(arr);
		assert same(arr, new int[] { 5 });
		arr = new int[] { 5, 6 };
		reverseInPlace(arr);
		assert same(arr, new int[] { 6, 5 });
		arr = new int[] { 5, 6, 7 };
		reverseInPlace(arr);
		assert same(arr, new int[] { 7, 6, 5 });

	}

	public static int[] reverseCopy(int[] arr) {
		int[] cp = copy(arr);
		reverseInPlace(cp);
		return cp;
	}

	public static void reverseCopyTest() {
		assert same(new int[] {}, reverseCopy(new int[] {}));
		assert same(new int[] { 4 }, reverseCopy(new int[] { 4 }));
		assert same(new int[] { 8, 6 }, reverseCopy(new int[] { 6, 8 }));
		assert same(new int[] { 5, -1, 2 }, reverseCopy(new int[] { 2, -1, 5 }));
		int[] arr = { 1, 2, 3, 4 };
		int[] cp = reverseCopy(arr);
		cp[0] = -9;
		assert arr[0] == 1;
	}
}
