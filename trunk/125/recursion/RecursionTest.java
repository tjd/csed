package recursion;

import java.util.Arrays;

/*
 
 <p>Loosely speaking, a recursive definition is one that is defined in terms of itself. For
 example:</p>
 
 <pre>
 S(1) = 1
 S(n) = n + S(n - 1),   for n > 1
 </pre>
 
 <p>The function S has been defined recursively. It consists of two rules: S(1) = 1, is called 
 the <em>base case</em>, and S(n) = n + S(n - 1) is called the <em>recursive case</em>.</p>
 
 <p>Calculating S(1) is easy: S(1) = 1 is simply given. To calcuate S(2), we set n = 2 in the 
 second rule and get S(2) = 2 + S(2 - 1) = 2 + S(1) = 2 + 1 = 3. To calculate S(3), we get 
 S(3) = 3 + S(2), and we just saw that S(2) = 3, so this becomes S(3) = 3 + 3 = 6.</p>
 
 <p>Indeed, if we know the value of S(n-1), then it is easy to calculate the value S(n) using 
 the second rule. However, suppose we want to calculate S(5) without knowing any smaller S 
 values. The trick is to repeatedly apply the second rule until we get down to S(1):</p>
 
 <pre>
 S(5) = 5 + S(4) 
 = 5 + (4 + S(3)) 
 = 5 + (4 + (3 + S(2))) 
 = 5 + (4 + (3 + (2 + S(1)))) 
 = 5 + (4 + (3 + (2 + 1)))
 = 15
 </pre>

 <p>So S(5) = 15. Using this recursive definition of S, there's no way to directly calculate S(5).
 We must go through earlier values step-by-step as shown. (For this particular recursive definition 
 there is a simple formula that will immediately calculate the answer, but in general there is no 
 guarantee that a formula even exists for recursive definitions.)</p>

 <p>Calculating recursive definitions is drudge work, and so you will be happy to know that it is
 easy to program Java to do the work. For example, here is S in Java:</p>

 <pre>
 public static int S(int n) {
     if (n == 1) {    // base case
         return 1;
     } else {
         return n + S(n - 1);  // recursive case
     }
 }
 </pre>

 <p>This is straightforward transcription of the recursive definition of S into Java, and it 
 works!</p>

 <p>Lets try another recursive definition:</p>

 <pre>
 A(1) = 1
 A(n) = 3 * A(n - 1) - 2
 </pre>

 <p>In Java:</p>

 <pre>
 public static int A(int n) {
     if (n == 1) {
         return 1;
     } else {
         return 3 * A(n - 1) - 2;
     }
 }
 </pre>

 <p>Surprisingly, A(n) = 1 no matter what n you give it! See the results for yourself using 
 this test code:</p>

 <pre>
 for(int i = 1; i <= 10; ++i) {
     System.out.printf("B(%s) = %s\n", i, B(i));
 }
 </pre>

 <p>This prints the following:</p>

 <pre>
 A(1) = 1
 A(2) = 1
 A(3) = 1
 A(4) = 1
 A(5) = 1
 A(6) = 1
 A(7) = 1
 A(8) = 1
 A(9) = 1
 A(10) = 1
 </pre>

 <p>A slight modification can give very different results:</p>

 <pre>
 B(1) = 2    
 B(n) = 3 * B(n - 1) - 2
 </pre>

 <p>The Java function is this:</p>

 <pre>
 public static int B(int n) {
     if (n == 1) {
         return 2;
     } else {
         return 3 * B(n - 1) - 2;
     }
 }
 </pre>

 <p>And the first ten values:</p>

 <pre>
 B(1) = 2
 B(2) = 4
 B(3) = 10
 B(4) = 28
 B(5) = 82
 B(6) = 244
 B(7) = 730
 B(8) = 2188
 B(9) = 6562
 B(10) = 19684
 </pre>

<p>It turns out that B(n) = 3^(n-1) + 1; determining a formula for a recursive definition
requires mathematics beyond the scope of this course. And even then, not all recursive 
definitions have a corresponding formula --- recursion is a powerful tool!</p>

 <p>Another useful recursive definition is for calculating powers of 2. Note that this starts 
 at 0:</p>

 <pre>
 T(0) = 1
 T(n) = 2 * T(n - 1)
 </pre>

 <p>This recursive definition defines 2^n (2 to the power of n), i.e. T(n) = 2^n.
 Indeed, more generally, we can define a^n as follows:</p>

 <pre>
 P(a, 0) = 1, for a != 0
 P(a, n) = a * T(n - 1), for n > 0
 </pre>
 
 <p>This defines a to the power of n. Notice how compact and simple the definition is.</p>

 <p>As with all mathematical definitions, it's important to follow the rules <em>precisely</em>. 
 For example, note that zero to the power of zero is undefined.</p>
 
 <p>So we can translate this to Java as follows:</p>

 <pre>
 public static int pow(int a, int n) {
     assert !(a == 0 && n == 0);
     if (a == 0) {
         return 0;
     } else if (n == 0) {
         return 1;
     } else {
         return a * pow(a, n - 1);
     }
 }
 </pre>

<p>One of the nice things about recursive definitions is that they are so easy to implement
in Java. The Java program corresponds pretty directly to the definition itself.</p>

 */

public class RecursionTest {

	public static int pow(int a, int n) {
		assert !(a == 0 && n == 0);
		if (a == 0) {
			return 0;
		} else if (n == 0) {
			return 1;
		} else {
			return a * pow(a, n - 1);
		}
	}

	public static void powTest() {
		// assert 1 == 2;
		assert pow(0, 1) == 0;
		assert pow(1, 0) == 1;
		assert pow(1, 1) == 1;
		assert pow(2, 3) == 8;
		assert pow(3, 2) == 9;
		System.out.printf("all powTests done\n");
	}

	public static int B(int n) {
		if (n == 1) {
			return 2;
		} else {
			return 3 * B(n - 1) - 2;
		}
	}

	public static int A(int n) {
		if (n == 1) {
			return 1;
		} else {
			return 3 * A(n - 1) - 2;
		}
	}

	public static int S(int n) {
		if (n == 1) {
			return 1;
		} else {
			return n + S(n - 1);
		}
	}

	public static int fib(int n) {
		if (n == 1 || n == 2) {
			return 1;
		} else {
			return fib(n - 2) + fib(n - 1);
		}
	}

	public static int fib2(int n) {
		if (n == 1 || n == 2) {
			return 1;
		} else {
			int a = 1;
			int b = 1;
			int c = a + b;
			for (int i = 0; i < n - 3; ++i) {
				a = b;
				b = c;
				c = a + b;
			}
			return c;
		}
	}

	public static void fibTest2() {
		assert fib2(1) == 1;
		assert fib2(2) == 1;
		assert fib2(3) == 2;
		assert fib2(4) == 3;
		assert fib2(5) == 5;
		assert fib2(6) == 8;
		assert fib2(7) == 13;
		System.out.printf("all fibTest2 tests passed\n");
	}

	public static void fibTest1() {
		assert fib(1) == 1;
		assert fib(2) == 1;
		assert fib(3) == 2;
		assert fib(4) == 3;
		assert fib(5) == 5;
		assert fib(6) == 8;
		assert fib(7) == 13;
		System.out.printf("all fibTest1 tests passed\n");
	}

	// returns the first element of arr
	public static int first(int[] arr) {
		return arr[0];
	}

	// Returns a new array that contains all the elements of arr except for the
	// first.
	// WARNING: This method makes a copy of arr (excluding the first element),
	// and this
	// takes extra space and time. For the sample functions below using this
	// rest
	// function is *not* the most efficient way to write that code. The
	// advantage of using
	// rest is to make the code simpler and easier to understand. Remember
	// Knuth's quote:
	//
	// Premature optimization is the root of all evil.
	//
	// If necessary, these functions can be made more efficient later on.
	public static int[] rest(int[] arr) {
		int[] result = new int[arr.length - 1];
		for (int i = 1; i < arr.length; ++i) {
			result[i - 1] = arr[i];
		}
		return result;
	}

	public static void testFirstRest() {
		assert first(new int[] { 5 }) == 5;
		assert first(new int[] { 4, 5 }) == 4;
		assert Arrays.equals(rest(new int[] { 1 }), new int[] {});
		assert Arrays.equals(rest(new int[] { 1, 2 }), new int[] { 2 });
		assert Arrays.equals(rest(new int[] { 1, 2, 3 }), new int[] { 2, 3 });
		System.out.printf("all testFirstRest tests passed\n");
	}

	public static int recMax(int[] arr) {
		if (arr.length == 1) {
			return first(arr);
		} else {
			return Math.max(first(arr), recMax(rest(arr)));
		}
	}

	public static void recMaxTest() {
		assert recMax(new int[] { 7 }) == 7;
		assert recMax(new int[] { 4, 7 }) == 7;
		assert recMax(new int[] { 7, 4 }) == 7;
		assert recMax(new int[] { 4, 7, 2 }) == 7;
		assert recMax(new int[] { 4, 7, 2, 54, 23, 54, 34, 2, 3, 5 }) == 54;
		System.out.printf("all recMaxTest tests passed\n");
	}

	public static int recSum(int[] arr) {
		if (arr.length == 1) {
			return first(arr);
		} else {
			return first(arr) + recSum(rest(arr));
		}
	}

	public static void recSumTest() {
		assert recSum(new int[] { 7 }) == 7;
		assert recSum(new int[] { 7, 2 }) == 9;
		assert recSum(new int[] { 7, 1, 0 }) == 8;
		System.out.printf("all recSumTest tests passed\n");
	}

	public static boolean recLinearSearch(int x, int[] arr) {
		if (arr.length == 1) {
			return first(arr) == x;
		} else {
			return (first(arr) == x) || recLinearSearch(x, rest(arr));
		}
	}

	public static boolean isSorted(int[] arr) {
		if (arr.length == 1) {
			return true;
		} else {
			int[] tail = rest(arr);
			return (first(arr) <= first(tail)) && isSorted(tail);
		}
	}

	public static void isSortedTest() {
		assert isSorted(new int[] { 5 });
		assert isSorted(new int[] { 2, 5 });
		assert isSorted(new int[] { 2, 5, 9, 44 });
		assert !isSorted(new int[] { 5, 4 });
		assert !isSorted(new int[] { 5, 6, 7, 9, 8, 10 });
		System.out.printf("all isSortedTest tests passed\n");
	}

	public static void test2() {
		testFirstRest();
		recMaxTest();
		isSortedTest();
		recSumTest();
		System.out.printf("done\n");
	}

	public static void test1() {
		powTest();
		fibTest1();
		fibTest2();
		fib(40);
		System.out.printf("done\n");
		fib2(40);
		System.out.printf("done\n");
		// for (int i = 1; i <= 10; ++i) {
		// System.out.printf("A(%s) = %s\n", i, A(i));
		// }
		//
		// for (int i = 1; i <= 10; ++i) {
		// System.out.printf("B(%s) = %s\n", i, B(i));
		// }
	}

	public static void main(String[] args) {
		test2();
	}

}
