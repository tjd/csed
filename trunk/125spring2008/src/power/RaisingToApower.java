package power;

public class RaisingToApower {

	public static void main(String[] args) {
		util.Util.ensureAssertionsEnabled();
		test1();
		test2();
		System.out.printf("All tests passed.\n");
	}

	private static void test2() {
		assert power(2, 0) == 1;
		assert power(2, 1) == 2;
		assert power(2, 2) == 4;
		assert power(2, 3) == 8;
		assert power(2, 4) == 16;
		assert power(2, 5) == 32;
		assert power(2, 6) == 64;
		assert power(2, 7) == 128;
		System.out.printf("All power tests passed.\n");
	}

	private static void test1() {
		assert simplePower(2, 0) == 1;
		assert simplePower(2, 1) == 2;
		assert simplePower(2, 2) == 4;
		assert simplePower(2, 3) == 8;
		assert simplePower(2, 4) == 16;
		assert simplePower(2, 5) == 32;
		assert simplePower(2, 6) == 64;
		assert simplePower(2, 7) == 128;
		System.out.printf("All simplePower tests passed.\n");
	}

	public static int simplePower(int a, int p) {
		assert p >= 0;
		if (p == 0) {
			return 1;
		} else {
			int result = a;
			for (int i = 1; i < p; ++i) {
				result = a * result;
			}
			return result;
		}
	}

	public static int power(int a, int p) {
		if (p == 0) {
			return 1;
		} else if (p % 2 == 1) { // p is odd
			return a * power(a * a, (p - 1) / 2);
		} else { // p is even
			return power(a * a, p / 2);
		}
	}

}
