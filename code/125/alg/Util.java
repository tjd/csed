package alg;

public class Util {

	// Returns a string version of arr with each element seperated by
	// sep.
	public static String join(String sep, int[] arr) {
		int n = arr.length;
		if (n == 0) {
			return "";
		} else if (n == 1) {
			return "" + arr[0];
		} else {
			String result = "" + arr[0];
			for (int i = 1; i < arr.length; ++i) {
				result += sep + arr[i];
			}
			return result;
		}
	}

	// Returns true iff a and b have the same elements in the same order.
	public static boolean same(int[] a, int[] b) {
		if (a.length != b.length) {
			return false;
		} else {
			int n = a.length;
			for (int i = 0; i < n; ++i) {
				if (a[i] != b[i]) {
					return false;
				}
			}
			return true;
		}
	}

	// Returns the dot product of a and b.
	public static int dotProduct(int[] a, int[] b) {
		assert a.length == b.length;
		int result = 0;
		for (int i = 0; i < a.length; ++i) {
			result += a[i] * b[i];
		}
		return result;
	}

	// Prints arr to the console window.
	public static void print(int[] arr) {
		System.out.printf("{%s}", join(", ", arr));
	}

	public static void main(String[] args) {
		int[] arr1 = { 45, 22, 3, 7, 2, 1 };
		int[] arr2 = { 45, 22, 3, 7, 2, 1 };
		int[] arr3 = { 1, 3, -3, 2 };
		print(arr1);
		assert same(arr1, arr2);
		assert !same(arr1, arr3);
		System.out.printf("\ndone");
	}

}
