package objects;

public class Coin1 {
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
	}
	
	public boolean isHeads() {
		return headsUp;
	}
	
	public boolean isTails() {
		return !isHeads();
	}
}
