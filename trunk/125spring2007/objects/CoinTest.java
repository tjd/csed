package objects;

import java.util.ArrayList;

public class CoinTest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		puzzle();
	}
	
	public static void puzzle() {
		ArrayList<Coin1> flippers = new ArrayList<Coin1>();
		
		for(int i = 0; i < 1000; ++i) {
			flippers.add(new Coin1());
		}
		
		int count = 0;
		while (flippers.size() > 0) {
			System.out.printf("%s: %s\n", count, flippers.size());
			flipAll(flippers);
			removeFlippers(flippers);
			++count;
		}
	}
	
	public static void flipAll(ArrayList<Coin1> flippers) {
		for(Coin1 c : flippers) {
			c.flip();
		}
	}
	
	public static ArrayList<Coin1> removeFlippers(ArrayList<Coin1> flippers) {
		ArrayList<Coin1> result = new ArrayList<Coin1>();
		for(Coin1 c : flippers) {
			if (c.isHeads()) {
				result.add(c);
			}
		}
		return result;
	}

}
