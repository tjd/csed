package diophantine;

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
		return a*a + b*b == c*c;
	}
	
	// Prints all Pythagorean triples (a, b, c) where each number is in
	// the range lo to hi (inclusive), and a <= b <= c.
	public static void printFromTo(int lo, int hi) {
		int count = 0;
		for(int a = lo; a <= hi; ++a) {
			for(int b = a; b <= hi; ++b) {
				for(int c = b; c <= hi; ++c) {
					if (isPythag(a, b, c)) {
						System.out.printf("(%s, %s, %s)  [%s]\n", a, b, c, ++count);
					}
				}
			}
		}
	}
	
	// Counts the number of Pythagorean triples such that
	//  lo <= a <= hi
	//   a <= b <= hi
	//   b <= c <= hi
	public static int countFromTo(int lo, int hi) {
		int count = 0;
		for(int a = lo; a <= hi; ++a) {
			for(int b = a; b <= hi; ++b) {
				for(int c = b; c <= hi; ++c) {
					if (isPythag(a, b, c)) {
						++count;
					}
				}
			}
		}
		return count;
	}
	
	public static void main(String[] args) {
		for(int hi = 10; hi <= 200; hi += 10) {
			System.out.printf("%s-%s: %s\n", 1, hi, countFromTo(1, hi));
		}
		System.out.println("done");
	}

}
