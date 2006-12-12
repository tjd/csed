package javaintro;

/*
 * Objects.py
 * 
 * Demonstrates object semantics.
 * 
 */

public class Objects {

	public static void main(String[] args) {
		String cat = "Newton";
		String dog = cat;

		System.out.println(cat + ", " + dog);

		dog = "Snickers";

		System.out.println(cat + ", " + dog);

		// shows the difference between == and .equals
		Integer a = new Integer(2534);
		Integer b = new Integer(2534);

		if (a == b) {
			System.out.println("a == b");
		} else {
			System.out.println("a != b");
		}

		if (a.equals(b)) {
			System.out.println("a.equals(b)");
		} else {
			System.out.println("!a.equals(b)");
		}

	}

}
