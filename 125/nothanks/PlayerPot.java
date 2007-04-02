package nothanks;

// This is used to restrict what players can see, i.e. players
// are allowed to see how many chips are in the pot, what card is up,
// and what cards other players have. But, for example, players cannot
// see how many chips other players have.

public interface PlayerPot {
	// return a copy of the cards all players currently hold
	public List<IntArray> getPlayerCards();

	// return a copy of the names of all the player
	public List<String> getPlayerNames();

	public int getNumChips();

	public int getNumCards();

	public int getUpCard();

	public boolean cardsLeft();

	public void printPlayerCards();
}
