package javaintro;

/*
 * 
 * Bits.java
 * 
 * Demonstrates bitwise operators.
 * 
 */

public class Bits {

	public static void main(String[] args) {
		// logical operators: and - &, or - |, exclusive or - |, not - ~
		System.out.printf("12 & 5 = %d\n", 12 & 5);
		System.out.printf("12 | 5 = %d\n", 12 | 5);
		System.out.printf("12 ^ 5 = %d\n", 12 ^ 5);
		System.out.printf("~12 = %d\n", ~12);

		// shift operators: shift left - <<, shift right - >>,
		// shift right fill zero - >>>
		System.out.printf("-5 >> 1 = %d\n", -5 >> 1);
		System.out.printf("-5 >> =%d\n2", -5 >> 2);
		System.out.printf("-5 >>> 1 = %d\n", -5 >>> 1);
		System.out.printf("-5 << 1 = %d\n", -5 << 1);

		//
		// example: pack 4 bytes into an int
		//
		byte alpha = 45, red = 4, green = 101, blue = 22;

		// pack in argb order
		int argb = (alpha << 24) + (red << 16) + (green << 8) + blue;

		// %x prints hexadecimal values, making it much easier to check
		// that the packing was done correctly
		System.out.printf("alpha = %x, red = %x, green = %x, blue = %x", alpha,
				red, green, blue);
		System.out.printf("\nargb = %x", argb);

	}

}
