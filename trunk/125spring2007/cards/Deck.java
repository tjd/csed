package cards;

import java.util.ArrayList;
import java.util.Arrays;

public class Deck {

	public int[] deck;

	public Deck(int numCards) {
		if (numCards <= 0 || numCards % 2 == 1) {
			throw new Error(
					"Deck initialization error: numCards must be a positive even number");
		}
		deck = new int[numCards];
		reset();
	}

	public void reset() {
		for (int i = 0; i < deck.length; ++i) {
			deck[i] = i;
		}
	}
	
	// the left half of the deck consists of all cards from 0
	// to splitPoint-1 inclusive; the right half is all the rest
	// Note: there are more elegant and efficient ways to do this,
	// but this code is straightforward, and so easy to understand
	// and debug
	public void cut(int splitPoint) {
		ArrayList<Integer> arr = new ArrayList<Integer>(deck.length);

		// first add all the elements from splitPoint onwards
		for (int i = splitPoint; i < deck.length; ++i) {
			arr.add(deck[i]);
		}

		// next add all the elements before splitPoint
		for (int i = 0; i < splitPoint; ++i) {
			arr.add(deck[i]);
		}

		// copy the re-arranged cards in arr back into the deck
		for (int i = 0; i < deck.length; ++i) {
			deck[i] = arr.get(i);
		}
	}

	// perfect riffle shuffle where the top card remains the top card
	public void perfectRiffleShuffleOut() {
		final int splitPoint = deck.length / 2;
		int a = 0;
		int b = splitPoint;
		ArrayList<Integer> arr = new ArrayList<Integer>(deck.length);
		for (int i = 0; i < deck.length; ++i) {
			if (i % 2 == 0) {
				arr.add(deck[a]);
				++a;
			} else {
				arr.add(deck[b]);
				++b;
			}
		}
		// copy the re-arranged cards in arr back into the deck
		for (int i = 0; i < deck.length; ++i) {
			deck[i] = arr.get(i);
		}
	}

	// perfect riffle shuffle where the to card remains the top card
	// becomes the second card
	public void perfectRiffleShuffleIn() {
		final int splitPoint = deck.length / 2;
		int a = 0;
		int b = splitPoint;
		ArrayList<Integer> arr = new ArrayList<Integer>(deck.length);
		for (int i = 0; i < deck.length; ++i) {
			if (i % 2 == 0) {
				arr.add(deck[b]);
				++b;
			} else {
				arr.add(deck[a]);
				++a;
			}
		}
		// copy the re-arranged cards in arr back into the deck
		for (int i = 0; i < deck.length; ++i) {
			deck[i] = arr.get(i);
		}
	}

	public void randomRiffleShuffle() {
		final int splitPoint = deck.length / 2;
		int a = 0;
		int b = splitPoint;
		ArrayList<Integer> arr = new ArrayList<Integer>(deck.length);
		for (int i = 0; i < deck.length; ++i) {
			if (a == splitPoint) {
				arr.add(deck[b]);
				b++;
			} else if (b == deck.length) {
				arr.add(deck[a]);
				a++;
			} else if (Math.random() < 0.5) {
				arr.add(deck[a]);
				a++;
			} else {
				arr.add(deck[b]);
				b++;
			}
		}
		// copy the re-arranged cards in arr back into the deck
		for (int i = 0; i < deck.length; ++i) {
			deck[i] = arr.get(i);
		}
	}

	public boolean inAscendingOrder() {
		for (int i = 1; i < deck.length; ++i) {
			if (deck[i - 1] >= deck[i]) {
				return false;
			}
		}
		return true;
	}

	public String toString() {
		return Arrays.toString(deck);
	}

}
