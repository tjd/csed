package power;

public class Power {

	// The naive power algorithm: multiplies a by itself n times.
	public static int powa(int a, int n) {
		assert !(a == 0 && n == 0);
		assert n >= 0;
		if (a == 0) {
			return 0;
		} else if (n == 0) {
			return 1;
		} else { // n > 1
			int p = a;
			for (int i = 0; i < n - 1; ++i) {
				p = p * a;
			}
			return p;
		}
	}

	// This algorithm usually does far fewer multiplications.
	public static int powb(int a, int n) {
		assert !(a == 0 && n == 0);
		assert n >= 0;
		if (n == 0) {
			return 1;
		} else if (n == 1) {
			return a;
		} else if (n % 2 == 0) {
			int s = powb(a, n / 2);
			return s * s;
		} else {
			return a * powb(a, n - 1);
		}
	}

	private static int mult_count;

	public static int powb_mult_count(int a, int n) {
		mult_count = 0;
		powb_counted(a, n);
		System.out.println();
		return mult_count;
	}

	public static int powb_counted(int a, int n) {
		// assert !(a == 0 && n == 0);
		assert n >= 0;
		if (n == 0) {
			return 1;
		} else if (n % 2 == 0) {
			++mult_count;
			System.out.print('1');
			int s = powb_counted(a, n / 2);
			return s * s;
		} else {
			++mult_count;
			System.out.print('0');
			return a * powb_counted(a, n - 1);
		}
	}

	public static void print_powb_mult_count() {
		for (int n = 1; n <= 30; ++n) {
			System.out.printf("2^%s: %s multiplications  ", n, powb_mult_count(
					2, n));
		}
	}

	public static void powa_test() {
		assert powa(1, 1) == 1;
		assert powa(0, 1) == 0;
		assert powa(1, 0) == 1;
		assert powa(5, 0) == 1;
		assert powa(5, 1) == 5;
		assert powa(5, 2) == 25;
		assert powa(5, 3) == 125;
		assert powa(5, 4) == 625;
	}

	public static void powb_test() {
		assert powb(1, 1) == 1;
		assert powb(0, 1) == 0;
		assert powb(1, 0) == 1;
		assert powb(5, 0) == 1;
		assert powb(5, 1) == 5;
		assert powb(5, 2) == 25;
		assert powb(5, 3) == 125;
		assert powb(5, 4) == 625;
	}

	public static void main(String[] args) {
		// Util.checkAssertions();
		// powa_test();
		// powb_test();
		// System.out.println("All tests passed.");
		print_powb_mult_count();
	}

}
