package cards;

import java.util.Random;

public class DeckTest {

	public static void main(String[] args) {
		test1();
		// test2();
	}

	public static void test1() {
		Deck deck = new Deck(10);
		System.out.println(deck);
		Random rnd = new Random();

		// do 10 random cuts
		for (int i = 0; i < 10; ++i) {
			int splitPoint = rnd.nextInt(deck.deck.length);
			deck.cut(splitPoint);
			System.out.println(deck);
		}
	}

	public static void test2() {
		Deck deck = new Deck(52);
		System.out.println(deck);
		System.out.println();

		// the following code counts how many perfect shuffles need be done
		// until the original order is returned
		int count = 0;
		deck.perfectRiffleShuffleOut();
		++count;
		while (!deck.inAscendingOrder()) {
			System.out.println(deck);
			deck.perfectRiffleShuffleOut();
			++count;
		}

		System.out.println(deck);
		System.out.println("" + count + " shuffles");
	}

	public static void test3() {
		Deck deck = new Deck(52);
		System.out.println(deck);
		System.out.println();
		deck.randomRiffleShuffle();
		System.out.println(deck);
	}
}
