package diophantine;

import java.util.ArrayList;
import java.util.Collections;

/*
 * (See http://en.wikipedia.org/wiki/Diophantus)
 * 
 * Diophantus of Alexandria was a Greek mathematician who lived sometime between
 * 150BC - 350AD. He's most famous for his study of equations whose variables take 
 * on rational values, and is considered by some to be the father of algebra.
 * 
 * Today, Diophantine equations refer to polynomials where the indeterminate variables
 * are restricted to integer values. For example, consider finding all triples of
 * integers (a, b, c) such that
 * 
 *    a^2 + b^2 = c^2
 *    
 * Integers that satisfy this equation are known as Pythagorean triples. While there
 * is some interesting mathematics underlying Pythagorean triples that could be used
 * to help us generate them more efficiently, we will ignore and simple try brute force:
 * we'll look for solutions by trying all possible values of a, b, and c in a given
 * range.
 * 
 */

public class BruteForce {

	public static boolean isPythag(int a, int b, int c) {
		return a * a + b * b == c * c;
	}

	// Prints all Pythagorean triples (a, b, c) where each number is in
	// the range lo to hi (inclusive), and a <= b <= c.
	public static void printFromTo(int lo, int hi) {
		int count = 0;
		for (int a = lo; a <= hi; ++a) {
			for (int b = a; b <= hi; ++b) {
				for (int c = b; c <= hi; ++c) {
					if (isPythag(a, b, c)) {
						System.out.printf("(%s, %s, %s)  [%s]\n", a, b, c,
								++count);
					}
				}
			}
		}
	}

	// Counts the number of Pythagorean triples such that
	// lo <= a <= hi
	// a <= b <= hi
	// b <= c <= hi
	public static int countFromTo(int lo, int hi) {
		int count = 0;
		for (int a = lo; a <= hi; ++a) {
			for (int b = a; b <= hi; ++b) {
				for (int c = b; c <= hi; ++c) {
					if (isPythag(a, b, c)) {
						++count;
					}
				}
			}
		}
		return count;
	}

	/*
	 * def prob39(): result = [] for p in xrange(1, 1000): count = 0 for a in
	 * xrange(1, p): for b in xrange(a + 1, p): c = p - a - b if a ** 2 + b ** 2 ==
	 * c ** 2: count += 1 result.append((count, p)) result.sort() return result
	 */

	public static void prob39() {
		ArrayList<Pair> result = new ArrayList<Pair>();
		for (int p = 1; p < 1000; ++p) {
			int count = 0;
			for (int a = 1; a < p; ++a) {
				for (int b = a; b < p; ++b) {
					int c = p - a - b;
					if (a * a + b * b == c * c) {
						++count;
					}
				}
			}
			result.add(new Pair(count, p));
		}
		System.out.println(Collections.max(result));
	}

	public static void main(String[] args) {
		// for(int hi = 10; hi <= 200; hi += 10) {
		// System.out.printf("%s-%s: %s\n", 1, hi, countFromTo(1, hi));
		// }
		long start = System.currentTimeMillis();
		prob39();
		long stop = System.currentTimeMillis();
		long diff = stop - start;
		System.out.println("done (" + diff + "ms)");
	}

}

class Pair implements Comparable<Pair> {
	int x, y;

	public Pair(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public int compareTo(Pair o) {
		if (this.x == o.x) {
			return 0;
		} else if (this.x < o.x) {
			return -1;
		} else {
			return 1;
		}
	}

	public String toString() {
		return String.format("(%s, %s)", x, y);
	}
}
