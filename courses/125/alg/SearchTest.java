package alg;

public class SearchTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		int[] arr = range(10000000);

		long fastStart = System.currentTimeMillis();
		int fast = fastLinearSearch(-1, arr);
		long fastStop = System.currentTimeMillis();
		System.out.printf("fast elapsed time in milliseconds: %sms\n", fastStop
				- fastStart);

		long slowStart = System.currentTimeMillis();
		int slow = slowLinearSearch(-1, arr);
		long slowStop = System.currentTimeMillis();
		System.out.printf("slow elapsed time: %sms\n", slowStop - slowStart);
	}

	public static int[] range(int n) {
		int[] arr = new int[n];
		for (int i = 0; i < n; ++i) {
			arr[i] = i;
		}
		return arr;
	}

	public static int slowLinearSearch(int x, int[] arr) {
		int i = 0;
		for (; i < arr.length; ++i) {
			if (arr[i] == x) {
				return -1;
			}
		}
		return i;
	}

	public static int fastLinearSearch(int x, int[] arr) {
		int n = arr.length;
		if (arr[n - 1] == x) {
			return n - 1;
		} else {
			int lastValue = arr[n - 1];
			arr[n - 1] = x;
			int i = 0;
			while (arr[i] != x) {
				++i;
			}
			arr[n - 1] = lastValue;
			return i % n;
		}
	}

}
