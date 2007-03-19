package nothanks;

public class RandomPlayer extends ComputerPlayer {

	public RandomPlayer() {
		super("Random" + getCount());
	}

	// Chooses its move by flipping a coin --- not a good
	// way to play!
	@Override
	public Move makeAmove(PlayerPot pot) {
		if (this.getNumChips() == 0) {
			return Move.takeIt;
		} else if (Math.random() <= 0.5) {
			return Move.takeIt;
		} else {
			return Move.noThanks;
		}
	}

}
