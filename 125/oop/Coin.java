package oop;

public class Coin {
	
	private double headProb;

	public Coin() {
		this(0.5);
	}
	
	public Coin(double probOfHeads) {
		assert 0 <= probOfHeads && probOfHeads <= 1;
		headProb = probOfHeads;
	}
	
	public double headProb() {
		return headProb;
	}
	
	public double tailProb() {
		return 1 - headProb;
	}
	
	public Side flip() {
		if (Math.random() <= headProb) {
			return Side.heads;
		} else {
			return Side.tails;
		}
	}
	

}
