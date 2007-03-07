package oop;

public class CoinTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Coin a = new Coin();
		Coin b = new Coin();

		System.out.println("a.flip() = " + a.flip());
		System.out.println("b.flip() = " + b.flip());
		doFlips(10);
	}

	public static void doFlips(int n) {
		Coin c = new Coin();
		int heads = 0;
		for (int i = 0; i < n; ++i) {
			if (c.flip().equals(Side.heads)) {
				++heads;
			}
		}
		int tails = n - heads;
		System.out.printf("Results of %s coin flips\n", n);
		System.out.println("heads = " + heads);
		System.out.println("tails = " + tails);
		System.out
				.println("Assuming heads wins $2 per flip, and tails only $1, then");
		System.out.printf("heads won $%s and tails $%s\n", 2 * heads - tails,
				tails - 2 * heads);
	}

}
