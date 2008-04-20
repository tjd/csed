package collatz;

public class Collatz {

	public static void main(String[] args) {
		System.out.println(tcp(10));
		System.out.println(tcp(11));
		System.out.println(repeat(20, 2));
		System.out.println(repeat(15, 3));
		System.out.println(repeat(15, -5));
		System.out.println(tcp(tcp(tcp(10))));
		System.out.println(repeat(11, tcp(5)));

		for (int i = 1; i <= 100; ++i) {
			System.out.println(i + ": " + collatz(i));
		}
	}

	public static int tcp(int n) {
		if (n == 1) {
			return 1;
		} else if (n % 2 == 0) {
			return n / 2;
		} else {
			return 3 * n + 1;
		}
	}

	public static int repeat(int start, int numTimes) {
		int result = start;
		for (int i = 0; i < numTimes; ++i) {
			result = tcp(result);
		}
		return result;
	}

	// It has been conjectured that the following function always returns 1
	// for any n >= 1. However, it has not been proven --- so no one knows if
	// this function always halts, or could result in an infinite loop.
	// Note: It has been empirically verified that this function returns 1 for
	// all values of n less than about 2.7 x 10^16, so strictly speaking all
	// possible 32-bit Java ints will return 1.
	public static int collatz(int n) {
		int count = 0;
		while (n != 1) {
			n = tcp(n);
			++count;
		}
		return count;
	}

}
