package nothanks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class MainNoThanks {

	public static void main(String[] args) {
		MainNoThanks game = new MainNoThanks();
		game.mainLoop();
	}
	
	public void mainLoop() {
		// main loop
		boolean playAgain = true;
		while (playAgain) {
			int n = getNumPlayersFromUser();
			ArrayList<Player> players = makePlayers(n);
			Pot pot = new Pot(players);

			int p = 0;
			while (pot.cardsLeft()) {
				Move move = playOneRound(pot, players.get(p));
				if (move.equals(Move.noThanks)) {
					p = (p + 1) % players.size();
				}
			}

			displayFinalScores(players);
			playAgain = askToPlayAgain();
		}
		System.out.printf("\nBye bye! Thanks for playing.\n");
	}

	public int getNumPlayersFromUser() {
		Scanner sc = new Scanner(System.in);
		System.out.print("How many opponents to you want? 2, 3, or 4? --> ");
		String r = sc.next().trim().toLowerCase();
		try {
			int n = Integer.parseInt(r);
			if (n >= 2 && n <= 4) {
				return n + 1;
			} else {
				return 4 + 1; // 4 computer players, one human player
			}
		} catch (NumberFormatException e) {
			return 4 + 1; // 4 computer players, one human player
		}

	}

	// add 1 human player and n-1 computer players
	public ArrayList<Player> makePlayers(int n) {
		ArrayList<Player> result = new ArrayList<Player>();
		result.add(new HumanPlayer());
		for (int i = 1; i < n; ++i) {
			result.add(ComputerPlayer.randomComputerPlayer());
			// result.add(new RandomPlayer("Randy" + i));
		}

		// randomly chosen players goes first
		Collections.shuffle(result);

		return result;
	}

	public Move playOneRound(Pot pot, Player p) {
		Move move = p.makeAmove(pot);
		if (move.equals(Move.noThanks)) {
			// the player doesn't want the card, so they add a chip
			System.out.printf("\n%s added a chip to the pot\n", p.getName());
			p.removeOneChip();
			pot.addOneChip();
		} else {
			// the player has taken the card!
			System.out.printf(
					"\n!! %s took the %s and %s chips (%s cards remain) !!\n",
					p.getName(), pot.getUpCard(), pot.getNumChips(), pot
							.getNumCards());
			p.addChips(pot.getNumChips());
			p.addCard(pot.getUpCard());
			pot.clearChips();
			if (pot.cardsLeft()) {
				pot.nextCard();
			}
		}
		return move;
	}

	// doesn't check for ties!
	public void displayFinalScores(ArrayList<Player> players) {
		IntArray scores = new IntArray();
		for (Player p : players) {
			scores.add(p.calculateScore());
		}
		int mi = scores.indexOfMax();
		System.out.printf("\n!!!! The winner is: %s! Score: %s !!!!\n\n",
				players.get(mi).getName(), scores.get(mi));

		// display final scores
		for (Player p : players) {
			System.out.printf("%s, Score = %s (%s, %s)\n", p.getName(), p
					.calculateScore(), p.getNumChips(), p.getCards());
		}
	}

	public boolean askToPlayAgain() {
		Scanner sc = new Scanner(System.in);
		System.out.print("Want to play again? (Y)es or (N)o? --> ");
		String r = sc.next().trim().toLowerCase();
		return r.length() > 0 && r.charAt(0) == 'y';
	}

	// Pot is private inner class so that Player objects cannot cast
	// PlayerPot to get at other player's chips, or the unviewed cards
	private class Pot implements PlayerPot {
		private int numChips;

		private IntArray deck;

		private List<IntArray> playerCards;

		private List<String> playerNames;

		public Pot(ArrayList<Player> players) {
			this.numChips = 0;
			makeDeck();
			playerCards = new List<IntArray>();
			playerNames = new List<String>();
			for (Player p : players) {
				playerCards.add(p.getCards());
				playerNames.add(p.getName());
			}
		}

		// return a copy of the cards all players currently hold
		public List<IntArray> getPlayerCards() {
			return playerCards.copy();
		}

		// return a copy of the names of all the player
		public List<String> getPlayerNames() {
			return playerNames.copy();
		}

		public void printPlayerCards() {
			for (int i = 0; i < playerCards.size(); ++i) {
				System.out.printf("%s cards: %s\n", playerNames.get(i),
						playerCards.get(i));
			}
		}

		public int getNumChips() {
			return numChips;
		}

		public int getNumCards() {
			return deck.size();
		}

		public void clearChips() {
			numChips = 0;
		}

		public void addOneChip() {
			++numChips;
		}

		public int getUpCard() {
			return deck.first();
		}

		public boolean cardsLeft() {
			return deck.size() > 0;
		}

		public void nextCard() {
			deck.remove(0);
		}

		private void makeDeck() {
			deck = new IntArray();

			// initialize the deck: cards go from 3 to 35
			for (int i = 3; i <= 35; ++i) {
				deck.add(i);
			}

			// mix up the cards
			deck.shuffle();

			// remove 9 cards
			for (int i = 0; i < 9; ++i) {
				deck.remove(deck.size() - 1);
			}
		}

	}

}
