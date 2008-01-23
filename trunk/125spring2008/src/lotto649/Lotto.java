package lotto649;

import java.util.ArrayList;
import java.util.Random;

public class Lotto {

	private static Random rnd;

	public static void main(String[] args) {			
		rnd = new Random();
		
		// fill an ArrayList with the numbers from 1 to 49
		ArrayList<Integer> choices = new ArrayList<Integer>();
		for (int i = 1; i <= 49; ++i) {
			choices.add(i);
		}

		// myPicks will hold the 6 chosen numbers
		ArrayList<Integer> myPicks = new ArrayList<Integer>();

		// randomly choose 6 numbers from the choices,
		// deleting each chosen number to ensure it is not
		// chosen again
		for (int i = 1; i <= 6; ++i) {
			int randIndex = rnd.nextInt(choices.size());
			int pick = choices.get(randIndex);
			myPicks.add(pick);
			choices.remove(randIndex);
		}

		System.out.printf("%s", myPicks);
	}

}
