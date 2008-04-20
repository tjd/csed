package javaintro;

/*
 * Wrappers.java
 * 
 * Demonstrates Java's wrapper classes.
 * 
 */

public class Wrappers {

	public static void main(String[] args) {
		System.out.println("an int is represented with " + Integer.SIZE
				+ " bits");
		System.out.println("Smallest integer: " + Integer.MIN_VALUE);
		System.out.println("Biggest integer: " + Integer.MAX_VALUE);

		System.out.println();

		System.out.println("a double is represented with " + Double.SIZE
				+ " bits");
		System.out.println("Smallest double: " + Double.MIN_VALUE);
		System.out.println("Biggest double: " + Double.MAX_VALUE);

		System.out.println();

		System.out.println("a char is represented with " + Character.SIZE
				+ " bits");

		// \u0000 is the min char value
		System.out.println("Smallest char: '" + Character.MIN_VALUE + "'");

		// \uffff is the max char value
		System.out.println("Biggest char: '" + Character.MAX_VALUE + "'");

		System.out.println();

		Integer n1 = new Integer(352);

		// autoboxing: new feature in Java 1.5
		Integer n2 = 352;

		System.out.println("n1 = " + n1 + "\nn2 = " + n2);

		// unboxing
		int n3 = n2;
		System.out.print("n3 = " + n3);
	}

}
