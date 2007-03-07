package oop.coin;

// Coin2 has the same methods names as Coin, but a different implementation.

public class Coin2 implements Coin {
	private Side side;

	public Coin2() {
		side = Side.heads;
	}

	public void flip() {
		if (Math.random() <= 0.5) {
			side = Side.heads;
		} else {
			side = Side.tails;
		}
	}

	public boolean isHeads() {
		return side == Side.heads;
	}
	
	public boolean isTails() {
		return !isHeads();
	}
	
	public Side side() {
		return side;
	}
}

// Side is an enumerated type, i.e. a finite set of values.
enum Side {
	heads, tails
}