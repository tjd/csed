package unittest;

/*
 * 
 * Unlucky 7
 * 
 * Write a function that prints the sum of all the integers from 1 to 1000 that 
 * don't contain the digit 7.
 * 
 * Hint: Write a helper function called hasSeven(int n) that returns true if n has a 7, and
 * false if it doesn't.
 * 
 * Digital Sum
 * 
 * Write a function that takes an int n as input returns the sum of the digits in n. For example,
 * if n = 4311, then the sum of its digits is 4 + 3 + 1 + 1 = 9. If n = -891, then your function
 * should return 8 + 9 + 1 = 18.
 * 
 * Crazy Sum
 * 
 * What's the sum of the numbers from 1 to 1000 that are *not* divisible by their digital
 * sum. For example, the sum of the digits of 414 is 9, and 414 is divisible by 9 (since 414 = 9 * 46),
 * and so 414 will *not* be part of the sum. 
 * 
 */

public class Sums {

	public static boolean hasSeven(int n) {
		return ("" + n).indexOf("7") != -1;
	}

	public static void hasSevenTest() {
		// assert 1 == 2;
		assert hasSeven(7);
		assert hasSeven(173);
		assert hasSeven(704);
		assert hasSeven(2047);
		assert hasSeven(77);
		assert hasSeven(777);
		assert hasSeven(7777);
		assert hasSeven(72047);
		assert !hasSeven(204);
		assert !hasSeven(0);
		assert !hasSeven(10);
		System.out.println("all hasSeven tests passed");
	}

	public static void prob1() {
		int sum = 0;
		for (int i = 1; i <= 1000; ++i) {
			if (!hasSeven(i)) {
				sum += i;
			}
		}
		System.out.printf("sum of 7-free numbers from 1 to a 1000: %s", sum);
	}

	public static int digitSum(int n) {
		n = Math.abs(n);
		int sum = 0;
		while (n > 0) {
			int r = n % 10;
			sum += r;
			n = (n - r) / 10;
		}
		return sum;
	}

	public static void digitSumTest() {
		assert digitSum(4) == 4;
		assert digitSum(0) == 0;
		assert digitSum(-4) == 4;
		assert digitSum(423) == 9 : digitSum(423);
		assert digitSum(-423) == 9;
		assert digitSum(43278) == (4 + 3 + 2 + 7 + 8);
		System.out.println("all digitSum tests passed");
	}

	public static void prob2() {
		int sum = 0;
		for (int i = 1; i <= 1000; ++i) {
			if ((i % digitSum(i)) != 0) {
				sum += i;
			}
		}
		System.out.printf(
				"\nsum of non-digit sum divisible numbers from 1 to 1000: %s",
				sum);
	}

	public static void main(String[] args) {
		hasSevenTest();
		digitSumTest();
		prob1();
		prob2();
	}

}
