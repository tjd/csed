package javaintro;

/*
 * 
 * Number.py 
 * 
 * Demonstrates some numeric subtleties.
 *
 */

public class Numbers {

	public static void main(String[] args) {
		// why does this print 0?
		System.out.println("3 / 5 = " + (3 / 5));

		System.out.println("3.0 / 5 = " + (3.0 / 5));

		// changing 127 to 128 causes a syntax error; why?
		byte b = 127;
		System.out.println("b = " + b);

		// Why does this print 128?
		System.out.println("b + 1 = " + (b + 1));

		// Why does this print -128?
		System.out.println("b + 1 = " + (byte) (b + 1));
	}
}
