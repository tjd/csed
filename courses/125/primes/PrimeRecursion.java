package primes;

public class PrimeRecursion {

	public static void main(String[] args) {
		isPrime1Test();
	}

	// returns true iff a number from d to n - 1 (inclusive) divides into n
	public static boolean divides(int n, int d) {
		if (n <= d || n <= 1) {
			return false;
		} else if (n % d == 0) {
			return true;
		} else {
			return divides(n, d + 1);
		}
	}

	public static boolean isPrime1(int n) {
		if (n <= 1) {
			return false;
		} else if (n == 2) {
			return true;
		} else {
			return divides(n, 2);
		}
	}

	private static void isPrime1Test() {
		assert !isPrime1(-1);
		assert !isPrime1(0);
		assert !isPrime1(1);
		assert isPrime1(2);
		assert isPrime1(3);
		assert !isPrime1(4);
		assert isPrime1(5);
		assert !isPrime1(16);
		assert !isPrime1(25);
		assert !isPrime1(55);
		assert isPrime1(101);
		assert !isPrime1(105);
		System.out.printf("all isPrime1 tests done\n");
	}
}
