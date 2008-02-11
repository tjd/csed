package galaxy;

import java.util.ArrayList;

public class TestBeing {

	public static void main(String[] args) {
		Being b = new Being("Bill Gates", 100, 20);
		b.print();

		Human h = new Human("George Bush", 100, 20);
		h.print();

		Martian m = new Martian("Marvin");
		m.print();

		Strogg s = new Strogg("Kane");
		s.print();

		System.out.println("\n============================================");

		ArrayList<Being> spaceship = new ArrayList<Being>();
		spaceship.add(b);
		spaceship.add(h);
		spaceship.add(m);
		spaceship.add(s);

		// give everyone on the ship a oxygen tank
		for (Being x : spaceship) {
			x.addPossession("oxygen tank");
		}

		// print them all out
		for (Being x : spaceship) {
			System.out.println(x.toString());
			// the toString() method that gets called depends on the type
			// of the object x --- it could be Being.toString or Human.toString,
			// or even some other toString() method if the class hierarchy
			// is later modified
		}

		System.out.printf("\nTotal passenger weight: %.1f kg",
				totalWeight(spaceship));
	}

	public static double totalWeight(ArrayList<Being> arr) {
		double result = 0;
		for (Being b : arr) {
			result += b.getWeightInKG();
		}
		return result;
	}

}
