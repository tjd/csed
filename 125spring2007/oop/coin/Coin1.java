package oop.coin;

public class Coin1 implements Coin {

	private boolean headsUp;

	public Coin1() {
		headsUp = true;
	}

	public void flip() {
		if (Math.random() <= 0.5) {
			headsUp = true;
		} else {
			headsUp = false;
		}
		// or just:
		// readsUp = Math.random() <= 0.5
	}

	public boolean isHeads() {
		return headsUp;
	}
	
	public boolean isTails() {
		return !isHeads();
	}
}
