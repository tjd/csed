package nothanks;

import java.util.Random;

public abstract class ComputerPlayer extends Player {

	// count is used to help name computer players
	private static int count = 1;

	public static int getCount() {
		return count;
	}

	private static Random rnd = new Random();

	public static ComputerPlayer randomComputerPlayer() {		
		int r = rnd.nextInt(2);
		if (r == 0) {
			return new RandomPlayer();
		} else if (r == 1) {
			return new NeverTakePlayer();
		}
		throw new Error("Error in randomComputerPlayer random number generation");
	}

	private final String name;

	public ComputerPlayer(String name) {
		++count;
		this.name = name;
	}

	@Override
	public String getName() {
		return name;
	}

}
