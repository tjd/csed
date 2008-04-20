package nothanks;

// The Player class is abstract, meaning that you cannot make Player objects.
// Instead, you write classes that extend it, and then create instance of
// those classes.

public abstract class Player {
	private int numChips;

	private IntArray cards;

	public Player() {
		// in the regular game, all players start with 11 chips
		this(11); 
	}

	public Player(int startingChips) {
		this.numChips = startingChips;
		this.cards = new IntArray();
	}

	public int getNumChips() {
		return numChips;
	}

	// When this is called, the player is given the current pot info,
	// and they make (and return) their move.
	abstract public Move makeAmove(PlayerPot pot);

	abstract public String getName();

	public void removeOneChip() {
		if (numChips > 0) {
			--numChips;
		}
	}

	public void addChips(int n) {
		numChips += n;
	}

	public void addCard(int card) {
		cards.add(card);
		cards.sort();
	}

	// the score is the number of chips minus the sum of the cards;
	// but in sequential runs of cards, only the lowest card value counts
	public int calculateScore() {
		int score = this.numChips - cards.sum();

		// now substract the value of all cards that occur in sequence,
		// except for the card that appears first in the sequence
		cards.sort();
		for (int i = 1; i < cards.size(); ++i) {
			int prev = cards.get(i - 1);
			int curr = cards.get(i);
			if (prev + 1 == curr) {
				score += curr;
			}
		}
		return score;
	}

	public IntArray getCards() {
		return cards;
	}

}
