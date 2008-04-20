package cards;

public class DeckTest {

	public static void main(String[] args) {
		test1();
		// test2();
	}

	public static void test1() {
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

	public static void test2() {
		Deck deck = new Deck(52);
		System.out.println(deck);
		System.out.println();
		deck.randomRiffleShuffle();
		System.out.println(deck);
	}
}
