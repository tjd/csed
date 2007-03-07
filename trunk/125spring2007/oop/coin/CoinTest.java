package oop.coin;

import java.util.ArrayList;

public class CoinTest {

	public static void main(String[] args) {
		flipOut(10);
	}

	// Suppose n people each flip a coin at the same time.
	// Those who get tails are out.
	// Those who get heads all flip again.
	// How many times do you expect this to be repeated?
	public static void flipOut(int n) {
		// initialize the list of flippers
		ArrayList<Coin> flippers = new ArrayList<Coin>();
		for(int i = 0; i < n; ++i) {
			flippers.add(new Coin2());
		}
		
		// perform the experiment
		int count = 0;
		while (!flippers.isEmpty()) {
			System.out.printf("%s: %s remain\n", count, flippers.size());
			flipAll(flippers);
			flippers = removeTails(flippers);
			++count;
		}
	}

	public static void flipAll(ArrayList<Coin> flippers) {
		for (Coin c : flippers) {
			c.flip();
		}
	}

	public static ArrayList<Coin> removeTails(ArrayList<Coin> flippers) {
		ArrayList<Coin> result = new ArrayList<Coin>();
		for (Coin c : flippers) {
			if (c.isHeads()) {
				result.add(c);
			}
		}
		return result;
	}

}
