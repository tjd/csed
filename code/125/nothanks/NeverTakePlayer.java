package nothanks;

public class NeverTakePlayer extends ComputerPlayer {

	public NeverTakePlayer() {
		super("Never" + getCount());
	}

	@Override
	public Move makeAmove(PlayerPot pot) {
		// never take a card, unless forced to;
		// this strategy is particularly foolish: it will not take a card even
		// when the number of chips is higher than the up card!
		if (this.getNumChips() == 0) {
			return Move.takeIt;
		} else {
			return Move.noThanks;
		}
	}

}
