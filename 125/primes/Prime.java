package primes;

/*
 * To learn the basic syntax and usage of Java, we'll implement a program that finds prime numbers.
 * 
 * Recall that a prime number is a positive integer n with exactly two divisors, 1 and n. The smallest
 * prime is 2, and then the next few primes are 3, 5, 7, 11, 13, .... There's an infinite number of
 * primes, and they turn out to have important applications in computer security.
 * 
 * Determining if a Number is Prime
 * --------------------------------
 * We want to deterimine if a given integer n is prime. We'll do this using a very general technique
 * called generate and test. Given an integer n that we want to test for primality, we simply
 * generate all the numbers from 2 to n - 1 and test if any of them divide into n.
 * 
 * Actually, before testing any divisors, we should take care of a few special cases. If n is 1 or 
 * less, then it's not prime, so we will check for that explicitly. The prime 2 is unusual not only
 * because it is the first prime, but it is the only even prime. It's often a good idea to handle
 * special cases like this explicitly, and so that's what we will do here.
 * 
 * So when n is 3 or greater we start generating divisors. We start by trying 2, then 3, then 4, then
 * 5, and so on, until either we find a divisors that goes into n with no remainder, or we reach
 * n - 1 without finding a divisors (in which case n must be prime).
 * 
 * The expression n % d returns the remainder when when d is divided into n. For example, 25 % 5 is 0,
 * and 33 % 6 is 3. Remember this: the % operator is often quite useful.
 * 
 * Testing our Function
 * --------------------
 * After we've written the isPrime function and gotten rid any syntax errors, we need to test the
 * function to make sure it really works correctly.
 * 
 * We'll test our function using a technique called "automatic unit testing". A unit test is a test
 * applied to a unit --- in this case, the unit is a single function (in general, a unit can be 
 * any sensible chunk of code that hangs together as a single piece).
 * 
 * The simplest way to do a unit test is to print the results of the function on the screen,
 * and verify the result by looking at it. Except that checking results manually soon becomes 
 * boring, and so it's a rather error-prone technique.
 * 
 * A much better technique is to have the computer check the results for us. We'll do this using
 * Java's assert statement. Consider this example:
 * 
 *    assert isPrime(2);  
 *    
 * This works by calling isPrime(2), and, if the result is true, then nothing happens --- the program
 * continues on to the following statement. If, however, isPrime(2) returns false, then the
 * program crashes with an AssertionError. Since 2 is prime the only time "assert isPrime(2);" 
 * should crash is when there's a bug in the isPrime function.
 * 
 * To check the result of isPrime(4), we write this:
 * 
 *    assert !isPrime(4);
 *    
 * We must include the ! (logical "not") because when isPrime works correctly, isPrime(4) should
 * evaluate to false. If we didn't include the ! then assert would crash when it returns the
 * correct result. So it is vital to include or exclude to the !, depending on the correct answer.
 * 
 * Good testing can be difficult. You should always try to find the minimum number of test 
 * cases that thoroughly tests your function. For the isPrime function, a good set of test cases
 * should test extreme values, i.e. values near boundaries or near special cases, and also a few
 * ordinary cases of number that are prime or not prime.  
 * 
 * See the isPrimeTest() for a list of the test cases.
 *   
 * Calculating the Number of Primes Up to N
 * ----------------------------------------
 * Now lets use the isPrime function to write another function that calculates the number of primes
 * from 1 to n (including n).
 * 
 * Once again the idea is to use generate and test: generate the numbers from 1 to n, call
 * isPrime on them, and keep track of the number that are prime.
 * 
 * As with isPrime, we will use if-statements to explicitly handle the special cases that arise
 * for small values of n.
 * 
 * The else part of the code test the numbers in reverse order, from n down to 2. The reason for
 * going in this order is to avoid creating another variable.
 * 
 * The ++ operator is called the increment operator, and it adds one to a variable. Similarly, -- is
 * the decrement operator, and it subtracts 1. These operators are not strictly necessary, but they
 * are convenient and popluar shorthand that is used frequently in Java (and C, and C++).
 * 
 * Once the function is written, we then create a suite of automatic unit tests. As with isPrime,
 * we test small integers values and a few cases for larger values.
 * 
 * Timing Functions
 * ----------------
 * Another common programming task is to measure the running time of a function. This is important
 * when you are comparing two or more functions that solve the same problem, and need to determine
 * which is faster.
 * 
 * We will calculate elapsed time using the built-in function System.currentTimeMillis(). This function
 * returns the total number of milliseconds that have elapsed since midnight, January 1st, 1970.
 * We'll call it once before the function we want to time, and then once after, and the difference
 * in the two times will be the running time of the function:
 * 
 *      long startTime = System.currentTimeMillis();
 *		int num = numPrimes(100000);
 *		long endTime = System.currentTimeMillis();
 *		double elapsedSeconds = (endTime - startTime) / 1000.0;
 * 
 * Notice that we store the result of System.currentTimeMillis() as a variable of type long.
 * That's because that's what System.currentTimeMillis() returns; long values consist of 64
 * bits, while int values consist of only 32. Thus, long's can store *many* more values than
 * ints.
 * 
 * When we calculate elapsedSeconds, notice two things: we divide by 1000.0, and we store the result
 * as a double. If we were to divide by 1000 (without the .0 on the end) then Java would do integer
 * division, and chop off any number to the right of the decimal point. Using 1000.0 forces Java
 * to do floating-point division, which preserves the decimal values. We store the result as
 * type double because double's are the standard way to store numbers with values to the right of
 * the decimal point. If we used int (or long) instead of double, then, again, Java would 
 * ruthlessly chop off and numbers to the right of the decinal point.
 * 
 * The value of 100000 was chosen in the second line since it resulted in a running time of about
 * 14 seconds (the value might be more or less for you, depending on the speed of your CPU and the
 * software you have running on your computer). If the run time is very short, then it is difficult
 * to get an accurate timimg; if it is very long, then you run the risk of losing interest!
 * 
 * Speeding Up numPrimes
 * ---------------------
 * It turns out that our numPrimes function is quite slow. To count the primes up to 100000 it takes
 * 12-15 seconds on my computer. From one point of view, this is pretty good: the computer is checking
 * 100000 numbers in only a few seconds.
 * 
 * However, it is possible to do *much* better. By applying a little bit of knowledge of prime
 * numbers, and the factors of non-prime numbers, we can re-write the isPrime function like this:
 * 
 * 	public static boolean isPrime2(int n) {
 *		if (n <= 1) {
 *			return false;
 *		} else if (n == 2) {
 * 			return true;
 * 		} else if (n % 2 == 0) {
 *			return false;
 * 		} else {
 * 			int d = 3;
 * 			while (d*d <= n) {
 * 				if (n % d == 0) {
 *					return false;
 *				}
 *				d = d + 2;
 *			}
 *			return true;
 *		}
 *	}
 *
 * First notice that a new "else if" clause has been added: it checks to see if n is evenly
 * divisible by 2 (recall that n % 2 returns the remainder when n is divided by 2; either 0 or 1
 * is returned, and if it is 0, then we know n is divisible by 2). This trick alone catches half
 * of all numbers we are likely to give to isPrime.
 * 
 * Second, look at the while loop condition: the loop only executes when d*d <= n, or, same thing,
 * when d is less than or equal to the square root of n. This trick relies of the fact that if, say,
 * n is divisible by a, then there must be some other number b such that n = a * b. In other words,
 * divisors of n always come in pairs, and we only have to check to see if n is divisible by one 
 * number of each pair. Thus, we only need to check up to the square root of n. This is a tremendous
 * savings when n is a prime number; in the original isPrime function, if n was prime then we
 * checked all the numners from 2 to n - 1. But here we only go from 2 to the square root of n.
 * For example, to test if 101 is prime, we only check need to check divisors that are less than
 * or equal to 10; in contrast, the original isPrime would check all the way up to 100. 
 * 
 * Third, notice that we initialize d to 3, and then increment it by 2 each time through the loop.
 * Since we know that n is odd, we don't need to bother checking if any even number divide into it.
 * So to prove that 101 is prime, the else part of the function does only four trial divisions: 3, 5, 7
 * and 9.
 * 
 * The results are dramatic: when the numPrimes function useds this new and improved isPrime2
 * function, it takes only 0.1 seconds, compared to 15 seconds for the original isPrime. In fact, 
 * it was so fast that when I first ran it I didn't even realize it had stopped --- I thought there
 * was a bug in the program that wasn't running the code!
 *
 */

public class Prime {

	public static void main(String[] args) {
		isPrime1Test();
		isPrime2Test();
		isPrime3Test();
		isPrime4Test();
		isPrime5Test();
		numPrimes1Test();
		numPrimes2Test();
		numPrimes3Test();
		numPrimes4Test();
		numPrimes5Test();
		numPrimes6Test();
		// numPrimes1Timing();
		// numPrimes2Timing();
		numPrimes3Timing();
		numPrimes4Timing();
		numPrimes5Timing();
		numPrimes6Timing();
	}

	public static boolean isPrime1(int n) {
		if (n <= 1) {
			return false;
		} else if (n == 2) {
			return true;
		} else {
			int d = 2;
			while (d < n) {
				if (n % d == 0) {
					return false;
				}
				d = d + 1;
			}
			return true;
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
		assert !isPrime1(55);
		assert isPrime1(101);
		assert !isPrime1(105);
		System.out.printf("all isPrime1 tests done\n");
	}

	public static boolean isPrime2(int n) {
		if (n <= 1) {
			return false;
		} else if (n == 2) {
			return true;
		} else if (n % 2 == 0) {
			return false;
		} else {
			int d = 3;
			while (d * d <= n) {
				if (n % d == 0) {
					return false;
				}
				d = d + 2;
			}
			return true;
		}
	}

	private static void isPrime2Test() {
		assert !isPrime2(-1);
		assert !isPrime2(0);
		assert !isPrime2(1);
		assert isPrime2(2);
		assert isPrime2(3);
		assert !isPrime2(4);
		assert isPrime2(5);
		assert !isPrime2(55);
		assert isPrime2(101);
		assert !isPrime2(105);
		System.out.printf("all isPrime2 tests done\n");
	}

	public static int[] SMALL_PRIMES = { 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 };

	// Returns true iff n is evenly divisible by one or more of SMALL_PRIMES
	public static boolean checkSmallPrime(int n) {
		int i = 0;
		while (i < SMALL_PRIMES.length) {
			if (n % SMALL_PRIMES[i] == 0) {
				return true;
			}
			++i;
		}
		return false;
	}

	public static boolean smallPrimesContains(int n) {
		int i = 0;
		while (i < SMALL_PRIMES.length) {
			if (SMALL_PRIMES[i] == n) {
				return true;
			}
			++i;
		}
		return false;
	}

	// Preliminary experiments show some interesting behaviour. When called with
	// numPrimes4, the isPrime3 function beats isPrime2 by about 50% for
	// 100000*10, but then
	// at 100000*10*10 both are about the same speed!
	public static boolean isPrime3(int n) {
		if (n <= 1) {
			return false;
		} else if (smallPrimesContains(n)) {
			return true;
		} else if (checkSmallPrime(n)) {
			return false;
		} else {
			int d = 2 + SMALL_PRIMES[SMALL_PRIMES.length - 1];
			while (d * d <= n) {
				if (n % d == 0) {
					return false;
				}
				d += 2;
			}
			return true;
		}
	}

	private static void isPrime3Test() {
		assert !isPrime3(-1);
		assert !isPrime3(0);
		assert !isPrime3(1);
		assert isPrime3(2);
		assert isPrime3(3);
		assert !isPrime3(4);
		assert isPrime3(5);
		assert !isPrime3(55);
		assert isPrime3(101);
		assert !isPrime3(105);
		System.out.printf("all isPrime3 tests done\n");
	}

	public static boolean isPrime4(int n) {
		if (n <= 1) {
			return false;
		} else {
			// first check for small primes
			int i = 0;
			while (i < SMALL_PRIMES.length) {
				int p = SMALL_PRIMES[i];
				if (n == p) {
					return true;
				} else if (n % p == 0) {
					return false;
				}
				++i;
			}

			// now generate odd divisors starting after the last small prime
			int d = 2 + SMALL_PRIMES[SMALL_PRIMES.length - 1];
			while (d * d <= n) {
				if (n % d == 0) {
					return false;
				}
				d += 2;
			}
			return true;
		}
	}

	private static void isPrime4Test() {
		assert !isPrime4(-1);
		assert !isPrime4(0);
		assert !isPrime4(1);
		assert isPrime4(2);
		assert isPrime4(3);
		assert !isPrime4(4);
		assert isPrime4(5);
		assert !isPrime4(55);
		assert isPrime4(101);
		assert !isPrime4(105);
		System.out.printf("all isPrime4 tests done\n");
	}

	public static boolean isPrime5(int n) {
		if (n <= 1) {
			return false;
		} else {
			// first check for small primes
			for(Integer p: SMALL_PRIMES) {
				if (n == p) {
					return true;
				} else if (n % p == 0) {
					return false;
				}
			}

			// now generate odd divisors starting after the last small prime
			int d = 2 + SMALL_PRIMES[SMALL_PRIMES.length - 1];
			while (d * d <= n) {
				if (n % d == 0) {
					return false;
				}
				d += 2;
			}
			return true;
		}
	}

	private static void isPrime5Test() {
		assert !isPrime5(-1);
		assert !isPrime5(0);
		assert !isPrime5(1);
		assert isPrime5(2);
		assert isPrime5(3);
		assert !isPrime5(4);
		assert isPrime5(5);
		assert !isPrime5(55);
		assert isPrime5(101);
		assert !isPrime5(105);
		System.out.printf("all isPrime5 tests done\n");
	}
	
	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes1(int n) {
		if (n <= 1) {
			return 0;
		} else {
			int count = 0;
			while (n > 1) {
				if (isPrime1(n)) {
					++count;
				}
				--n;
			}
			return count;
		}
	}

	public static void numPrimes1Test() {
		assert numPrimes1(-1) == 0;
		assert numPrimes1(0) == 0;
		assert numPrimes1(1) == 0;
		assert numPrimes1(2) == 1;
		assert numPrimes1(3) == 2;
		assert numPrimes1(4) == 2;
		assert numPrimes1(5) == 3;
		assert numPrimes1(25) == 9;
		assert numPrimes1(26) == 9;
		assert numPrimes1(27) == 9;
		assert numPrimes1(28) == 9;
		assert numPrimes1(29) == 10;
		System.out.printf("all numPrimes1 tests done\n");
	}

	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes2(int n) {
		if (n <= 1) {
			return 0;
		} else if (n == 2) {
			return 1;
		} else if (n == 3) {
			return 2;
		} else {
			int count = 2;
			int i = 5;
			while (i <= n) {
				if (isPrime1(i)) {
					++count;
				}
				i += 2;
			}
			return count;
		}
	}

	public static void numPrimes2Test() {
		assert numPrimes2(-1) == 0;
		assert numPrimes2(0) == 0;
		assert numPrimes2(1) == 0;
		assert numPrimes2(2) == 1;
		assert numPrimes2(3) == 2;
		assert numPrimes2(4) == 2;
		assert numPrimes2(5) == 3;
		assert numPrimes2(25) == 9 : numPrimes2(25);
		assert numPrimes2(26) == 9;
		assert numPrimes2(27) == 9;
		assert numPrimes2(28) == 9;
		assert numPrimes2(29) == 10;
		System.out.printf("all numPrimes2 tests done\n");
	}

	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes3(int n) {
		if (n <= 1) {
			return 0;
		} else if (n == 2) {
			return 1;
		} else if (n == 3) {
			return 2;
		} else {
			int count = 2;
			int i = 5;
			while (i <= n) {
				if (isPrime2(i)) {
					++count;
				}
				i += 2;
			}
			return count;
		}
	}

	public static void numPrimes3Test() {
		assert numPrimes3(-1) == 0;
		assert numPrimes3(0) == 0;
		assert numPrimes3(1) == 0;
		assert numPrimes3(2) == 1;
		assert numPrimes3(3) == 2;
		assert numPrimes3(4) == 2;
		assert numPrimes3(5) == 3;
		assert numPrimes3(25) == 9 : numPrimes2(25);
		assert numPrimes3(26) == 9;
		assert numPrimes3(27) == 9;
		assert numPrimes3(28) == 9;
		assert numPrimes3(29) == 10;
		System.out.printf("all numPrimes3 tests done\n");
	}

	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes4(int n) {
		if (n <= 1) {
			return 0;
		} else if (n == 2) {
			return 1;
		} else if (n == 3) {
			return 2;
		} else {
			int count = 2;
			int i = 5;
			while (i <= n) {
				if (isPrime3(i)) {
					++count;
				}
				i += 2;
			}
			return count;
		}
	}

	public static void numPrimes4Test() {
		assert numPrimes4(-1) == 0;
		assert numPrimes4(0) == 0;
		assert numPrimes4(1) == 0;
		assert numPrimes4(2) == 1;
		assert numPrimes4(3) == 2;
		assert numPrimes4(4) == 2;
		assert numPrimes4(5) == 3;
		assert numPrimes4(25) == 9 : numPrimes2(25);
		assert numPrimes4(26) == 9;
		assert numPrimes4(27) == 9;
		assert numPrimes4(28) == 9;
		assert numPrimes4(29) == 10;
		System.out.printf("all numPrimes4 tests done\n");
	}

	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes5(int n) {
		if (n <= 1) {
			return 0;
		} else if (n == 2) {
			return 1;
		} else if (n == 3) {
			return 2;
		} else {
			int count = 2;
			int i = 5;
			while (i <= n) {
				if (isPrime4(i)) {
					++count;
				}
				i += 2;
			}
			return count;
		}
	}

	public static void numPrimes5Test() {
		assert numPrimes5(-1) == 0;
		assert numPrimes5(0) == 0;
		assert numPrimes5(1) == 0;
		assert numPrimes5(2) == 1;
		assert numPrimes5(3) == 2;
		assert numPrimes5(4) == 2;
		assert numPrimes5(5) == 3;
		assert numPrimes5(25) == 9 : numPrimes2(25);
		assert numPrimes5(26) == 9;
		assert numPrimes5(27) == 9;
		assert numPrimes5(28) == 9;
		assert numPrimes5(29) == 10;
		System.out.printf("all numPrimes5 tests done\n");
	}

	// Returns the numnber of primes less than or equal to n.
	public static int numPrimes6(int n) {
		if (n <= 1) {
			return 0;
		} else if (n == 2) {
			return 1;
		} else if (n == 3) {
			return 2;
		} else {
			int count = 2;
			int i = 5;
			while (i <= n) {
				if (isPrime5(i)) {
					++count;
				}
				i += 2;
			}
			return count;
		}
	}
	
	public static void numPrimes6Test() {
		assert numPrimes6(-1) == 0;
		assert numPrimes6(0) == 0;
		assert numPrimes6(1) == 0;
		assert numPrimes6(2) == 1;
		assert numPrimes6(3) == 2;
		assert numPrimes6(4) == 2;
		assert numPrimes6(5) == 3;
		assert numPrimes6(25) == 9 : numPrimes2(25);
		assert numPrimes6(26) == 9;
		assert numPrimes6(27) == 9;
		assert numPrimes6(28) == 9;
		assert numPrimes6(29) == 10;
		System.out.printf("all numPrimes6 tests done\n");
	}
	
	final static int MAX_NUM = 100000 * 10 * 10 * 2;

	public static void numPrimes1Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes1(%s) ...\n", MAX_NUM);
		int num = numPrimes1(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes1(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}

	public static void numPrimes2Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes2(%s) ...\n", MAX_NUM);
		int num = numPrimes2(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes2(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}

	public static void numPrimes3Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes3(%s) ...\n", MAX_NUM);
		int num = numPrimes3(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes3(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}

	public static void numPrimes4Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes4(%s) ...\n", MAX_NUM);
		int num = numPrimes4(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes4(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}

	public static void numPrimes5Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes5(%s) ...\n", MAX_NUM);
		int num = numPrimes5(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes5(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}
	
	public static void numPrimes6Timing() {
		long startTime = System.currentTimeMillis();
		System.out.printf("Running numPrimes6(%s) ...\n", MAX_NUM);
		int num = numPrimes5(MAX_NUM);
		long endTime = System.currentTimeMillis();
		double elapsedSeconds = (endTime - startTime) / 1000.0;
		System.out.printf("numPrimes6(%s) = %s\n%.1f seconds\n", MAX_NUM, num,
				elapsedSeconds);
	}
}
